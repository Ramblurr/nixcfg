{ inputs, pkgs }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  mkGuest =
    extra:
    inputs.self.lib.nixcfg.mkGuest "zvol-guest" {
      hostPath = ./fixtures/zvol-guest;
      extraModules = [ extra ];
    };
  v1 = mkGuest { };
  v2 = mkGuest (
    { pkgs, ... }:
    {
      environment.etc."moot-release".text = "v2";
      # A symlink store path: its target does not resolve in the initrd.
      environment.etc."moot-hello-link".source =
        pkgs.runCommand "moot-hello-link" { }
          "ln -s ${pkgs.hello} $out";
      environment.systemPackages = [ pkgs.hello ];
    }
  );
  v2Switch = v2.config.microvm.deploy.sshSwitch;
  v2Runner = v2.config.microvm.declaredRunner;
  zvolSize = v1.config.modules.microvm-guest.writableStoreOverlay.zvolSize;
in
pkgs.testers.runNixOSTest {
  name = "microvm-zvol-overlay";
  node.specialArgs = { inherit inputs; };

  nodes.host = {
    imports = [
      inputs.microvm.nixosModules.host
      inputs.impermanence.nixosModules.impermanence
      ../modules/microvm-host
    ];
    modules.microvm-host = {
      enable = true;
      baseZfsDataset = "tank/microvms";
    };
    microvm.vms.zvol-guest = {
      evaluatedConfig = v1;
      autostart = false;
    };
    # Under load the runner's socat notify relay (-T0.2) can drop READY=1,
    # which fails the start after the timeout. The test waits for SSH instead.
    systemd.services."microvm@zvol-guest".serviceConfig.Type = pkgs.lib.mkForce "simple";

    boot.supportedFilesystems = [ "zfs" ];
    networking.hostId = "8425e349";
    # The host module orders guest interfaces after networkd-wait-online.
    networking.useNetworkd = true;
    virtualisation = {
      cores = 2;
      memorySize = 3072;
      emptyDiskImages = [ 4096 ];
      # cloud-hypervisor needs KVM inside the test VM.
      qemu.options = [
        "-cpu"
        "host"
      ];
      # deploy copies the switch helper's closure to the VM host first.
      additionalPaths = [
        v2Switch
        v2Runner
      ];
    };
    environment.etc."microvm-test-key" = {
      source = sshKeys.snakeOilEd25519PrivateKey;
      mode = "0600";
    };
    programs.ssh.extraConfig = ''
      Host vsock-mux/*
        User root
        IdentityFile /etc/microvm-test-key
        BatchMode yes
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
    '';
  };

  testScript = ''
    import shlex

    state = "/var/lib/microvms/zvol-guest"
    zvol = "tank/microvms/zvol-guest/nix-rw"
    target = f"root@vsock-mux/{state}/notify.vsock"
    # -n: ssh must not read the test driver's command channel.
    ssh = f"ssh -n {target}"
    v1_top = "${v1.config.system.build.toplevel}"
    v2_top = "${v2.config.system.build.toplevel}"
    hello = "${v2.pkgs.hello}"
    hello_link = "${v2.config.environment.etc."moot-hello-link".source}"


    def guest(cmd):
        return host.succeed(f"{ssh} {shlex.quote(cmd)}")


    def guest_fail(cmd):
        host.fail(f"{ssh} {shlex.quote(cmd)}")


    def name(path):
        return path.removeprefix("/nix/store/")


    def upper_store_paths():
        return set(guest("ls -A /nix/.rw-store/store").split()) - {".links"}


    def closure(path):
        return {name(p) for p in guest(f"nix-store -qR {path}").split()}


    def referenced():
        host.succeed("zpool sync tank")
        return int(host.succeed(f"zfs get -Hp -o value referenced {zvol}"))


    def boot_guest():
        host.wait_for_unit("microvm@zvol-guest.service")
        host.wait_until_succeeds(f"{ssh} true", timeout=300)
        return guest("cat /proc/sys/kernel/random/boot_id").strip()


    host.start()
    host.wait_for_unit("multi-user.target")
    host.succeed("zpool create -O mountpoint=none tank /dev/vdb")
    host.succeed("systemctl start microvm@zvol-guest.service")
    first_boot = boot_guest()

    with subtest("the host provisions a sparse labelled zvol from the runner manifest"):
        assert int(host.succeed(f"zfs get -Hp -o value volsize {zvol}")) == ${toString zvolSize} << 20
        host.succeed(f"test $(zfs get -H -o value refreservation {zvol}) = none")
        host.succeed(f"test $(zfs get -H -o value volmode {zvol}) = dev")
        host.succeed(f"test $(blkid -s LABEL -o value /dev/zvol/{zvol}) = nix-rw")
        host.succeed(f"test $(readlink {state}/nix-rw.zvol) = /dev/zvol/{zvol}")
        host.succeed(f"grep -q free_page_reporting=on {state}/current/bin/microvm-run")

    with subtest("the guest stacks the erofs image under the zvol and shares nothing from the host"):
        guest("findmnt -n -o FSTYPE /nix/.ro-store | grep -qx erofs")
        guest("findmnt -n -o FSTYPE /nix/.rw-store | grep -qx ext4")
        opts = guest("findmnt -n -o FSTYPE,OPTIONS /nix/store")
        assert opts.startswith("overlay "), opts
        for want in ["lowerdir=/sysroot/nix/.ro-store", "upperdir=/sysroot/nix/.rw-store/store"]:
            assert want in opts, f"{want} missing from {opts}"
        guest("test /nix/var/nix/db/db.sqlite -ef /nix/.rw-store/var/nix/db/db.sqlite")
        shares = guest("findmnt -n -t 9p,virtiofs -o TARGET || true").split()
        assert shares == [], f"unexpected host shares: {shares}"
        guest("ls /sys/bus/virtio/drivers/virtio_balloon | grep -q '^virtio'")

    with subtest("the first boot registers the image closure and verifies the database"):
        guest("systemctl is-active nix-store-verify.service")
        guest(f"nix-store --check-validity {v1_top}")
        guest("systemctl is-enabled nix-gc.timer fstrim.timer")
        assert upper_store_paths() == set(), f"a fresh boot wrote to the upper layer: {upper_store_paths()}"

    with subtest("fstrim returns freed zvol blocks to the pool"):
        before = referenced()
        guest("head -c 256M /dev/urandom > /nix/.rw-store/trim-probe && sync")
        filled = referenced()
        assert filled - before > 200 << 20, f"{before} -> {filled}"
        guest("rm /nix/.rw-store/trim-probe && sync && systemctl start fstrim.service")
        trimmed = referenced()
        assert filled - trimmed > 200 << 20, f"{filled} -> {trimmed}"

    with subtest("the deploy switch helper hot-switches the guest without a reboot"):
        added = guest("echo moot-state > /tmp/moot-state && nix-store --add /tmp/moot-state").strip()
        host.succeed(f"${v2Switch}/bin/microvm-switch {target}")
        assert guest("cat /proc/sys/kernel/random/boot_id").strip() == first_boot, "the guest rebooted"
        guest("grep -qx v2 /etc/moot-release")
        guest("hello")
        guest(f"test $(readlink -f /nix/var/nix/profiles/system) = {v2_top}")
        new = closure(v2_top) - closure(v1_top)
        for p in [hello, hello_link]:
            assert name(p) in new, f"{p} is already in the v1 image"
        assert new <= upper_store_paths(), f"missing from the upper layer: {new - upper_store_paths()}"
        guest(f"test -L /nix/.rw-store/store/{name(hello_link)}")

    with subtest("a second switch to the same system copies nothing"):
        out = host.succeed(f"${v2Switch}/bin/microvm-switch {target} 2>&1")
        assert "Copying" not in out, out

    with subtest("the guest reboots onto the v2 image with the same zvol"):
        host.succeed(f"ln -sfn ${v2Runner} {state}/current")
        host.succeed("systemctl restart microvm@zvol-guest.service")
        assert boot_guest() != first_boot
        guest(f"test $(readlink /run/booted-system) = {v2_top}")
        guest("systemctl is-active nix-store-verify.service")

    with subtest("the initrd dropped upper copies of paths the v2 image ships"):
        left = upper_store_paths() & closure(v2_top)
        assert left == set(), f"duplicated in the upper layer: {left}"
        guest(f"test -e /nix/.ro-store/{name(hello)}")
        # microvm.nix's image builder stores a symlink path as a copy of its target.
        guest(f"test -x /nix/.ro-store/{name(hello_link)}/bin/hello")
        guest("hello")

    with subtest("state only the zvol holds survives and the database is consistent"):
        assert name(added) in upper_store_paths()
        guest(f"grep -qx moot-state {added}")
        guest(f"nix-store --check-validity {v2_top} {hello} {added}")
        report = guest("journalctl -b -u nix-store-verify.service")
        assert f"path '{v1_top}' disappeared, removing from database" in report, report
        guest_fail(f"nix-store --check-validity {v1_top}")
        report = guest("nix-store --verify 2>&1")
        assert "disappeared" not in report, report
  '';
}
