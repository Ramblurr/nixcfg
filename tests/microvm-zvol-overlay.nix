{ inputs, pkgs }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  mkGuest =
    extra:
    inputs.self.lib.nixcfg.mkGuest "zvol-guest" {
      hostPath = ./fixtures/zvol-guest;
      extraModules = [ extra ];
    };
  # v1 and v2 ship this path; v3 drops it. The hot-switched v2 system in the
  # upper layer refers to it while only the v1 image holds it.
  retired = {
    environment.etc."moot-retired".text = "retired";
  };
  helloRelease =
    { pkgs, ... }:
    {
      environment.etc."moot-release".text = "v2";
      # A symlink store path: its target does not resolve in the initrd.
      environment.etc."moot-hello-link".source =
        pkgs.runCommand "moot-hello-link" { }
          "ln -s ${pkgs.hello} $out";
      environment.systemPackages = [ pkgs.hello ];
    };
  v1 = mkGuest retired;
  v2 = mkGuest {
    imports = [
      retired
      helloRelease
    ];
  };
  v3 = mkGuest helloRelease;
  v2Switch = v2.config.microvm.deploy.sshSwitch;
  v3Runner = v3.config.microvm.declaredRunner;
  zvolSize = v1.config.modules.microvm-guest.writableStoreOverlay.zvolSize;
in
pkgs.testers.runNixOSTest {
  name = "microvm-zvol-overlay";
  node.specialArgs = { inherit inputs; };

  nodes.host = {
    imports = [
      inputs.microvm.nixosModules.host
      inputs.impermanence.nixosModules.impermanence
      ../modules/motd.nix
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
        v3Runner
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
    import json
    import shlex

    state = "/var/lib/microvms/zvol-guest"
    zvol = "tank/microvms/zvol-guest/nix-rw"
    target = f"root@vsock-mux/{state}/notify.vsock"
    # -n: ssh must not read the test driver's command channel.
    ssh = f"ssh -n {target}"
    v1_top = "${v1.config.system.build.toplevel}"
    v2_top = "${v2.config.system.build.toplevel}"
    v3_top = "${v3.config.system.build.toplevel}"
    retired = "${v1.config.environment.etc."moot-retired".source}"
    hello = "${v2.pkgs.hello}"
    hello_link = "${v2.config.environment.etc."moot-hello-link".source}"
    nixpkgs_src = "${v1.config.nix.registry.nixpkgs.to.path}"
    nixpkgs_version = "${v1.pkgs.lib.version}"
    shell = "${v1.pkgs.runtimeShell}"


    def guest(cmd):
        return host.succeed(f"{ssh} {shlex.quote(cmd)}")


    def check_nix(stage):
        for entry in ["nixpkgs", "nixos"]:
            found = guest(f"nix-instantiate --find-file {entry}").strip()
            assert found == nixpkgs_src, f"<{entry}> is {found}, not {nixpkgs_src}"
        version = guest("nix-instantiate --eval -E 'with import <nixpkgs> {}; lib.version'").strip()
        assert version == f'"{nixpkgs_version}"', version
        registry = guest("nix --extra-experimental-features 'nix-command flakes' --option flake-registry ''' registry list")
        assert f"system flake:nixpkgs path:{nixpkgs_src}?" in registry, registry
        guest(f"nix-store --check-validity {nixpkgs_src}")
        guest("systemctl is-active nix-store-verify.service")
        report = guest("nix-store --verify 2>&1")
        assert "disappeared" not in report, report
        expr = (
            f'derivation {{ name = "moot-{stage}"; system = builtins.currentSystem;'
            f' builder = builtins.storePath "{shell}"; args = [ "-c" "echo {stage} > $out" ]; }}'
        )
        out = guest(f"NIX_REMOTE=daemon nix-build --no-out-link -E {shlex.quote(expr)}").strip()
        assert guest(f"cat {out}").strip() == stage


    def guest_fail(cmd):
        host.fail(f"{ssh} {shlex.quote(cmd)}")


    def name(path):
        return path.removeprefix("/nix/store/")


    def upper_store_paths():
        return set(guest("ls -A /nix/.rw-store/store").split()) - {".links"}


    def closure(path):
        return {name(p) for p in guest(f"nix-store -qR {path}").split()}


    def staging_roots():
        return [r for r in guest("ls -A /nix/var/nix/gcroots").split() if r.startswith("microvm-switch.")]


    def referenced():
        host.succeed("zpool sync tank")
        return int(host.succeed(f"zfs get -Hp -o value referenced {zvol}"))


    def boot_guest():
        host.wait_for_unit("microvm@zvol-guest.service")
        host.wait_until_succeeds(f"{ssh} true", timeout=300)
        # SSH can answer before the boot's oneshot units finish.
        guest("systemctl is-system-running --wait || true")
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
        host.succeed(f"test $(zfs get -H -o value -s local org.nixcfg:formatted {zvol}) = true")
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

    with subtest("the v1 image resolves <nixpkgs> to its pinned nixpkgs and builds"):
        check_nix("v1")

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
        guest(f"ln -s {added} /nix/var/nix/gcroots/moot-state")
        # hello is valid on the guest but unrooted, so only the switch keeps it from GC.
        host.succeed(f"nix-store --export {hello} | ssh {target} nix-store --import")
        # Free space below min-free makes nix-store --import run a full auto-GC
        # while the switch is in progress.
        guest("mkdir -p ~/.config/nix && echo min-free = 1125899906842624 > ~/.config/nix/nix.conf")
        out = host.succeed(f"timeout 600 ${v2Switch}/bin/microvm-switch {target} 2>&1")
        guest("rm ~/.config/nix/nix.conf")
        assert "running auto-GC" in out, out
        assert staging_roots() == [], staging_roots()
        # The profile now roots hello.
        host.succeed(f"timeout 300 {ssh} nix-store --gc")
        guest(f"nix-store --check-validity {hello}")
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
        assert staging_roots() == [], staging_roots()

    with subtest("the hot-switched v2 system resolves <nixpkgs> and builds"):
        check_nix("v2")

    with subtest("the guest reboots onto the v3 image with the same zvol"):
        assert name(retired) in closure(v2_top) and name(retired) not in upper_store_paths()
        host.succeed(f"ln -sfn ${v3Runner} {state}/current")
        host.succeed("systemctl restart microvm@zvol-guest.service")
        assert boot_guest() != first_boot
        guest(f"test $(readlink /run/booted-system) = {v3_top}")
        guest("systemctl is-active nix-store-verify.service")

    with subtest("the initrd dedup ran once, before the overlay was mounted"):
        # systemd's "unit starting" and "unit started" catalog messages, in journal order.
        events = [
            (e["UNIT"], {"7d4958e842da4a758f6c1cdc7b36dcc5": "starting", "39f53479d3a045ac8e11786248231fbf": "started"}[e["MESSAGE_ID"]])
            for e in map(json.loads, guest(
                "journalctl -b -o json --output-fields=UNIT,MESSAGE_ID"
                " MESSAGE_ID=7d4958e842da4a758f6c1cdc7b36dcc5 MESSAGE_ID=39f53479d3a045ac8e11786248231fbf"
                " UNIT=nix-rw-store-dedup.service UNIT=sysroot-nix-store.mount"
            ).splitlines())
        ]
        assert events == [
            ("nix-rw-store-dedup.service", "starting"),
            ("nix-rw-store-dedup.service", "started"),
            ("sysroot-nix-store.mount", "starting"),
            ("sysroot-nix-store.mount", "started"),
        ], events

    with subtest("the initrd dropped upper copies of paths the v3 image ships with the same type"):
        left = upper_store_paths() & closure(v3_top)
        # microvm.nix's image builder stores a symlink path as a copy of its target,
        # so the upper symlink is the only copy that matches its database hash.
        assert left == {name(hello_link)}, f"duplicated in the upper layer: {left}"
        guest(f"test -d /nix/.ro-store/{name(hello_link)} && test ! -L /nix/.ro-store/{name(hello_link)}")
        guest(f"test -L /nix/store/{name(hello_link)}")
        guest(f"test -e /nix/.ro-store/{name(hello)}")
        guest("hello")

    with subtest("state only the zvol holds survives and the database is consistent"):
        assert name(added) in upper_store_paths()
        guest(f"grep -qx moot-state {added}")
        guest(f"nix-store --check-validity {v3_top} {hello} {added}")
        guest(f"nix-store -qR {v3_top} | xargs ls -d > /dev/null")
        for gone in [v1_top, retired, v2_top]:
            guest_fail(f"nix-store --check-validity {gone}")
        assert name(v2_top) not in upper_store_paths()
        report = guest("nix-store --verify --check-contents 2>&1")
        assert "disappeared" not in report and "modified" not in report, report

    with subtest("the v3 image resolves <nixpkgs> and builds after the reboot"):
        check_nix("v3")

    with subtest("nix-daemon starts on its socket only after the database is verified"):
        guest("systemctl stop nix-daemon.service")
        guest(f"NIX_REMOTE=daemon nix-store --check-validity {v3_top}")
        guest("systemctl is-active nix-daemon.service")
        guest(
            "mkdir -p /run/systemd/system/nix-store-verify.service.d"
            " && printf '[Service]\\nExecStart=\\nExecStart=/run/current-system/sw/bin/false\\n'"
            " > /run/systemd/system/nix-store-verify.service.d/fail.conf"
            " && systemctl daemon-reload"
        )
        guest_fail("systemctl restart nix-store-verify.service")
        guest_fail("systemctl start nix-daemon.service")
        guest_fail("systemctl is-active nix-daemon.service")

    with subtest("the host records a format an interrupted run did not record"):
        host.succeed("systemctl stop microvm@zvol-guest.service")
        uuid = host.succeed(f"blkid -p -s UUID -o value /dev/zvol/{zvol}").strip()
        host.succeed(f"zfs set org.nixcfg:formatted=false {zvol}")
        host.succeed("systemctl restart microvm-zfs-datasets@zvol-guest.service")
        host.succeed(f"test $(zfs get -H -o value -s local org.nixcfg:formatted {zvol}) = true")
        assert host.succeed(f"blkid -p -s UUID -o value /dev/zvol/{zvol}").strip() == uuid, "the zvol was reformatted"
        host.succeed(f"mkdir -p /mnt && mount -o ro,noload /dev/zvol/{zvol} /mnt")
        host.succeed(f"test -e /mnt/store/{name(added)} && umount /mnt")

    with subtest("the host never formats a zvol it did not create"):
        host.succeed(f"zfs destroy {zvol}")
        host.succeed(f"zfs create -s -V ${toString zvolSize}M -o volmode=dev {zvol}")
        host.succeed("systemctl restart microvm-zfs-datasets@zvol-guest.service")
        host.succeed(f"test -b /dev/zvol/{zvol}")
        # blkid exits 2 when it finds no signature.
        host.succeed(f"rc=0; blkid -p /dev/zvol/{zvol} || rc=$?; test $rc -eq 2")
        host.succeed(f"test $(zfs get -H -o value org.nixcfg:formatted {zvol}) = -")
        host.succeed(f"mkfs.ext4 -q -L other /dev/zvol/{zvol} && zfs set org.nixcfg:formatted=false {zvol}")
        host.fail("systemctl restart microvm-zfs-datasets@zvol-guest.service")
        host.succeed(f"test $(blkid -p -s LABEL -o value /dev/zvol/{zvol}) = other")
        host.succeed(f"test $(zfs get -H -o value -s local org.nixcfg:formatted {zvol}) = false")
  '';
}
