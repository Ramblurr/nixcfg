{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  diskConfig = import ../hosts/peirce/disk-config.nix { inherit lib pkgs; };
  # toFile needs materialized store references, including with lazy source trees.
  diskoSource = builtins.path {
    path = inputs.disko-unstable;
    name = "disko-test-source";
  };
  # TODO: Remove this harness adaptation once Disko supports the removed initrd option.
  # Related upstream issue: https://github.com/nix-community/disko/issues/1303
  # Rewrite in memory to avoid import-from-derivation during check evaluation.
  diskoTestLib =
    import
      (builtins.toFile "disko-systemd-initrd-tests.nix" (
        lib.replaceStrings
          [
            "../."
            "../module.nix"
            "config.boot.initrd.systemd.enable"
          ]
          [
            "${diskoSource}/."
            "${diskoSource}/module.nix"
            ''(lib.versionAtLeast config.system.nixos.release "26.11" || config.boot.initrd.systemd.enable)''
          ]
          (builtins.readFile "${diskoSource}/lib/tests.nix")
      ))
      {
        inherit lib;
        makeTest = import (pkgs.path + "/nixos/tests/make-test-python.nix");
        eval-config = import (pkgs.path + "/nixos/lib/eval-config.nix");
        qemu-common-lib =
          pkgs:
          import (pkgs.path + "/nixos/lib/qemu-common.nix") {
            inherit lib;
            inherit (pkgs) stdenv;
          };
      };
in
diskoTestLib.makeDiskoTest {
  inherit pkgs;
  name = "peirce-mirrored-boot";
  disko-config = diskConfig;
  extraInstallerConfig = {
    networking.hostId = "8ab491d2";
    virtualisation.memorySize = 2048;
    virtualisation.emptyDiskImages = lib.mkForce [
      8192
      8192
    ];
  };
  extraSystemConfig = {
    imports = [ ../hosts/peirce/hardware.nix ];
    networking.hostId = "8ab491d2";
    boot.loader.systemd-boot.enable = lib.mkForce false;
    boot.loader.grub.devices = lib.mkForce [ ];
    boot.zfs.requestEncryptionCredentials = true;
    boot.zfs.forceImportRoot = false;
  };
  postDisko = ''
    installer_machine = machine
    machine.succeed("cmp -n 65 /dev/disk/by-partlabel/peirce-cryptkey-crucial /dev/disk/by-partlabel/peirce-cryptkey-samsung")
  '';
  extraTestScript = ''
    import subprocess

    with subtest("both disks boot through EFI and unlock ZFS unattended"):
        machine.wait_for_unit("multi-user.target")
        machine.succeed("test $(zpool list -H -o health rpool) = ONLINE")
        machine.succeed("test $(zfs get -H -o value keystatus rpool/encrypted) = available")
        machine.succeed("mountpoint /boot/crucial; mountpoint /boot/samsung")
        machine.succeed("test -f /boot/crucial/EFI/BOOT/BOOTX64.EFI; test -f /boot/samsung/EFI/BOOT/BOOTX64.EFI")
        machine.succeed("echo persistent > /persist/boot-proof; touch /root-must-disappear")
        machine.succeed("sync")
        machine.shutdown()

    for index, survivor in enumerate(["crucial", "samsung"]):
        with subtest(f"unattended EFI boot with only {survivor}"):
            # Each degraded boot gets an overlay on the same clean baseline.
            # The missing SSD (including its EFI and key partitions) is not attached.
            overlay = installer_machine.state_dir / f"survivor-{survivor}.qcow2"
            subprocess.run([
                "${pkgs.qemu_test}/bin/qemu-img", "create", "-f", "qcow2",
                "-F", "qcow2", "-b", str(installer_machine.state_dir / f"empty{index}.qcow2"),
                str(overlay),
            ], check=True)

            def disks(oldmachine, num_disks):
                return ["-drive", f"file={overlay},id=survivor,if=none,format=qcow2",
                        "-device", "virtio-blk-pci,drive=survivor"]

            machine = create_test_machine(oldmachine=installer_machine, name=f"only_{survivor}")
            machine.start()
            machine.wait_for_unit("multi-user.target", timeout=240)
            machine.succeed("test $(zpool list -H -o health rpool) = DEGRADED")
            machine.succeed("test $(zfs get -H -o value keystatus rpool/encrypted) = available")
            machine.succeed(f"mountpoint /boot/{survivor}")
            machine.succeed("grep -qx persistent /persist/boot-proof; test ! -e /root-must-disappear")
            machine.succeed("test $(stat -c %a /run/peirce-zfs.key) = 400")
            machine.shutdown()
  '';
}
