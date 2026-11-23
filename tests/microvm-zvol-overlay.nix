{ pkgs }:
let
  inherit (pkgs) lib;

  # A store path that is a symlink. The initrd dedup must see it in the
  # image even though its target does not resolve before the overlay is up.
  helloLink = pkgs.runCommand "moot-hello-link" { } "ln -s ${pkgs.hello} $out";

  # The guest layout of a Moot agent or builder VM. qemu-vm.nix builds the
  # erofs store image from the node's own closure on every start, passes
  # regInfo on the kernel command line, and loads it with nix-store --load-db
  # on every boot, as microvm.nix registerClosure does. The writable layer is
  # an ext4 disk the host formatted once, standing in for the sparse zvol, and
  # /nix/var lives on it so db.sqlite outlives the image.
  guest = {
    # Runs between mounting the image and the writable layer and mounting
    # the overlay. A path the new image ships no longer needs its upper copy.
    boot.initrd.systemd.services.nix-rw-store = {
      description = "Prepare the writable Nix store layer";
      requiredBy = [
        "sysroot-nix-store.mount"
        "sysroot-nix-var.mount"
      ];
      before = [
        "sysroot-nix-store.mount"
        "sysroot-nix-var.mount"
      ];
      unitConfig = {
        DefaultDependencies = false;
        RequiresMountsFor = [
          "/sysroot/nix/.ro-store"
          "/sysroot/nix/.rw-store"
        ];
      };
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p /sysroot/nix/.rw-store/var
        for p in /sysroot/nix/.rw-store/store/*; do
          ro=/sysroot/nix/.ro-store/''${p##*/}
          if [ -e "$ro" ] || [ -L "$ro" ]; then rm -rf "$p"; fi
        done
      '';
    };

    boot.loader.grub.enable = false;
    system.switch.enable = true;
    nix.settings.experimental-features = [ "nix-command" ];
    environment.systemPackages = [ pkgs.sqlite ];

    virtualisation = {
      diskImage = null;
      useNixStoreImage = true;
      writableStore = true;
      writableStoreUseTmpfs = false;
      vlans = [ ];
      qemu.drives = [
        {
          name = "nix-rw";
          file = ''"$TMPDIR"/nix-rw.img'';
          driveExtraOpts.format = "raw";
          driveExtraOpts.werror = "report";
        }
      ];
      fileSystems = {
        "/nix/.rw-store" = {
          device = "/dev/disk/by-label/nix-rw";
          fsType = "ext4";
          neededForBoot = true;
        };
        # microvm.nix's writableStoreOverlay layout.
        "/nix/store".overlay = {
          upperdir = lib.mkForce "/nix/.rw-store/store";
          workdir = lib.mkForce "/nix/.rw-store/work";
        };
        "/nix/var" = {
          device = "/nix/.rw-store/var";
          fsType = "none";
          options = [ "bind" ];
          depends = [ "/nix/.rw-store" ];
          neededForBoot = true;
        };
      };
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "microvm-zvol-overlay";
  # Each store image holds only its own node's closure, so v2's paths reach
  # v1 only through the hot switch.
  includeTestScriptReferences = false;

  # v1 boots first and hot-switches to v2's system. The guest then reboots
  # onto v2's image with the same writable disk.
  nodes.v1 =
    { nodes, ... }:
    {
      imports = [ guest ];
      # The hot switch copies v2's closure from this cache, so the guest never
      # sees the host store.
      virtualisation.sharedDirectories.v2-cache = {
        source = "${pkgs.mkBinaryCache { rootPaths = [ nodes.v2.system.build.toplevel ]; }}";
        target = "/mnt/v2-cache";
      };
    };
  nodes.v2 = {
    imports = [ guest ];
    environment.etc."moot-release".text = "v2";
    environment.etc."moot-hello-link".source = helloLink;
    environment.systemPackages = [ pkgs.hello ];
  };

  testScript =
    { nodes, ... }:
    ''
      import shutil
      import subprocess

      v1_top = "${nodes.v1.system.build.toplevel}"
      v2_top = "${nodes.v2.system.build.toplevel}"
      hello = "${pkgs.hello}"
      hello_link = "${helloLink}"


      def name(path):
          return path.removeprefix("/nix/store/")


      # Store paths in the writable layer, leaving out Nix's own .links.
      def upper(machine):
          return set(machine.succeed("ls /nix/.rw-store/store").split())


      def closure(machine, path):
          return {name(p) for p in machine.succeed(f"nix-store -qR {path}").split()}


      # The host formats the sparse volume once, before the guest first boots.
      rw_img = v1.state_dir / "nix-rw.img"
      with open(rw_img, "wb") as f:
          f.truncate(2 << 30)
      subprocess.run(["${pkgs.e2fsprogs}/bin/mkfs.ext4", "-q", "-F", "-L", "nix-rw", str(rw_img)], check=True)

      with subtest("first boot mounts the erofs image under an ext4 writable layer"):
          v1.start()
          v1.wait_for_unit("multi-user.target")
          v1.succeed("findmnt -n -o FSTYPE /nix/.ro-store | grep -qx erofs")
          v1.succeed("findmnt -n -o FSTYPE /nix/.rw-store | grep -qx ext4")
          opts = v1.succeed("findmnt -n -o FSTYPE,OPTIONS /nix/store")
          assert opts.startswith("overlay "), opts
          for want in ["lowerdir=/sysroot/nix/.ro-store", "upperdir=/sysroot/nix/.rw-store/store", "workdir=/sysroot/nix/.rw-store/work"]:
              assert want in opts, f"{want} missing from {opts}"
          v1.succeed("test /nix/var/nix/db/db.sqlite -ef /nix/.rw-store/var/nix/db/db.sqlite")
          shares = set(v1.succeed("findmnt -n -t 9p,virtiofs -o TARGET").split())
          assert shares == {"/tmp/xchg", "/tmp/shared", "/mnt/v2-cache"}, f"unexpected host shares: {shares}"

      with subtest("first boot registers the v1 closure in a fresh db"):
          v1.succeed("systemctl is-active register-nix-paths.service")
          v1.succeed(f"nix-store --check-validity {v1_top}")
          v1.succeed("nix-store --verify")
          assert upper(v1) == set(), f"a fresh boot wrote to the upper layer: {upper(v1)}"

      with subtest("a hot switch writes v2's new paths into the writable layer"):
          v1.succeed(f"nix copy --no-check-sigs --from file:///mnt/v2-cache {v2_top}")
          v1.succeed(f"nix-env -p /nix/var/nix/profiles/system --set {v2_top}")
          v1.succeed(f"{v2_top}/bin/switch-to-configuration switch")
          v1.succeed("grep -qx v2 /etc/moot-release")
          v1.succeed("hello")
          v2_new = closure(v1, v2_top) - closure(v1, v1_top)
          for p in [v2_top, hello, hello_link]:
              assert name(p) in v2_new, f"{p} is already in the v1 image"
          assert v2_new <= upper(v1), f"missing from the upper layer: {v2_new - upper(v1)}"
          v1.succeed(f"test -L /nix/.rw-store/store/{name(hello_link)}")

      with subtest("a path added on the guest lands only in the writable layer"):
          added = v1.succeed("echo moot-state > /tmp/moot-state && nix-store --add /tmp/moot-state").strip()
          v1.succeed(f"test -f /nix/.rw-store/store/{name(added)}")
          v1.fail(f"test -e /nix/.ro-store/{name(added)}")
          v1.succeed(f"nix-store --verify-path {added}")
          v1.shutdown()

      with subtest("the guest reboots onto the v2 image with the same writable disk"):
          shutil.move(rw_img, v2.state_dir / "nix-rw.img")
          v2.start()
          v2.wait_for_unit("multi-user.target")
          v2.succeed(f"test $(readlink /run/booted-system) = {v2_top}")
          v2.succeed("systemctl is-active register-nix-paths.service")

      with subtest("the initrd dropped upper copies of paths the v2 image ships"):
          left = upper(v2) & closure(v2, v2_top)
          assert left == set(), f"duplicated in the upper layer: {left}"
          for p in [v2_top, hello, hello_link]:
              v2.succeed(f"test -e /nix/.ro-store/{name(p)} -o -L /nix/.ro-store/{name(p)}")
          v2.succeed("hello")
          v2.succeed(f"test $(readlink /nix/store/{name(hello_link)}) = {hello}")

      with subtest("a path only the writable layer holds survives intact"):
          assert upper(v2) == {name(added)}, f"upper layer holds {upper(v2)}"
          v2.succeed(f"grep -qx moot-state {added}")
          v2.succeed(f"nix-store --verify-path {added}")

      with subtest("db.sqlite stays consistent and registers the v2 closure"):
          v2.succeed("sqlite3 /nix/var/nix/db/db.sqlite 'PRAGMA integrity_check' | grep -qx ok")
          v2.succeed(f"nix-store --check-validity {v2_top} {hello} {hello_link} {added}")
          v2.succeed(f"nix-store --verify-path {hello}")

      # The v1 system was only ever in the v1 image, so it is gone, but its
      # rows still say valid. Nix would skip copying such a path on a later
      # switch that needs it again.
      with subtest("rows for paths only the v1 image held stay valid until nix-store --verify"):
          v2.fail(f"test -e {v1_top}")
          v2.succeed(f"nix-store --check-validity {v1_top}")
          report = v2.succeed("nix-store --verify 2>&1")
          assert f"path '{v1_top}' disappeared, removing from database" in report, report
          v2.fail(f"nix-store --check-validity {v1_top}")
          report = v2.succeed("nix-store --verify 2>&1")
          assert "disappeared" not in report, report
          v2.succeed(f"nix-store --check-validity {v2_top} {added}")
    '';
}
