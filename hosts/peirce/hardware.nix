{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot = {
    zfs.devNodes = lib.mkForce "/dev/disk/by-partuuid";
    loader = {
      efi.canTouchEfiVariables = false;
      # GRUB copies kernels and boot configuration to both ESPs.
      grub = {
        enable = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        copyKernels = true;
        configurationLimit = 20;
        mirroredBoots = [
          {
            path = "/boot/crucial";
            devices = [ "nodev" ];
          }
          {
            path = "/boot/samsung";
            devices = [ "nodev" ];
          }
        ];
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
      systemd = {
        enable = true;
        # No raw /dev/mapper/cryptkey: systemd must see a regular key file
        # before ZFS import. See work item 004 and the quine/mali migrations.
        services.peirce-zfs-key = {
          description = "Read the ZFS key from either mirror member";
          unitConfig.DefaultDependencies = false;
          after = [ "systemd-udev-trigger.service" ];
          wants = [ "systemd-udev-trigger.service" ];
          path = [
            pkgs.coreutils
            pkgs.gnugrep
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            UMask = "0077";
            TimeoutStartSec = "40s";
          };
          # Requiring both .device units would defeat degraded-mirror boot.
          script = ''
            for attempt in $(seq 1 30); do
              for disk in crucial samsung; do
                keypart=/dev/disk/by-partlabel/peirce-cryptkey-$disk
                if [ -b "$keypart" ] &&
                   head -c 65 "$keypart" > /run/peirce-zfs.key &&
                   [ "$(wc -c < /run/peirce-zfs.key)" -eq 65 ] &&
                   LC_ALL=C grep -Eq '^[0-9a-f]{64}$' /run/peirce-zfs.key; then
                  chmod 0400 /run/peirce-zfs.key
                  exit 0
                fi
              done
              sleep 1
            done
            rm -f /run/peirce-zfs.key
            echo "No readable ZFS key on either peirce mirror member" >&2
            exit 1
          '';
        };
        services.zfs-import-rpool = {
          requires = [ "peirce-zfs-key.service" ];
          after = [ "peirce-zfs-key.service" ];
        };
        services.rollback = {
          description = "Rollback ZFS datasets to a pristine state";
          wantedBy = [ "initrd.target" ];
          before = [ "sysroot.mount" ];
          path = [ config.boot.zfs.package ];
          unitConfig.DefaultDependencies = "no";
          serviceConfig.Type = "oneshot";
          requires = [ "zfs-import-rpool.service" ];
          after = [ "zfs-import-rpool.service" ];
          # The shared module also rolls back local/home, which peirce lacks.
          script = lib.mkForce ''
            zfs rollback -r rpool/encrypted/local/root@blank
            zfs rollback -r rpool/encrypted/vms@blank
          '';
        };
      };
    };
  };
}
