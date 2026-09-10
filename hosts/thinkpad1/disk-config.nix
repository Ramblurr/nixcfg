{ config, ... }:
let
  mountOptions = [
    "compress=zstd"
    "noatime"
  ];
in
{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = config.repo.secrets.local.systemDisk;
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            priority = 1;
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" ];
            };
          };
          encrypted = {
            priority = 2;
            size = "100%";
            content = {
              type = "luks";
              name = "cryptroot";
              extraFormatArgs = [
                "--type"
                "luks2"
              ];
              # Disko prompts for a recovery passphrase during installation.
              settings.crypttabExtraOpts = [ "tpm2-device=auto" ];
              content = {
                type = "lvm_pv";
                vg = "thinkpad1";
              };
            };
          };
        };
      };
    };
    lvm_vg.thinkpad1 = {
      type = "lvm_vg";
      lvs = {
        swap = {
          priority = 1;
          size = "24G";
          content = {
            type = "swap";
            resumeDevice = true;
            # The enclosing LUKS volume preserves the key across reboots.
            randomEncryption = false;
          };
        };
        system = {
          priority = 2;
          size = "100%FREE";
          content = {
            type = "btrfs";
            subvolumes = {
              "@root" = {
                mountpoint = "/";
                inherit mountOptions;
              };
              "@home" = {
                mountpoint = "/home";
                inherit mountOptions;
              };
              "@nix" = {
                mountpoint = "/nix";
                inherit mountOptions;
              };
              "@log" = {
                mountpoint = "/var/log";
                inherit mountOptions;
              };
              "@home-snapshots" = {
                mountpoint = "/home/.snapshots";
                inherit mountOptions;
              };
            };
          };
        };
      };
    };
  };

  boot.initrd.systemd.enable = true;
  boot.initrd.systemd.tpm2.enable = true;
  fileSystems."/home".neededForBoot = true;
  fileSystems."/nix".neededForBoot = true;

  services.snapper = {
    snapshotInterval = "hourly";
    persistentTimer = true;
    configs.home = {
      SUBVOLUME = "/home";
      FSTYPE = "btrfs";
      TIMELINE_CREATE = true;
      TIMELINE_CLEANUP = true;
      TIMELINE_LIMIT_HOURLY = 24;
      TIMELINE_LIMIT_DAILY = 7;
      TIMELINE_LIMIT_WEEKLY = 4;
      TIMELINE_LIMIT_MONTHLY = 3;
      TIMELINE_LIMIT_QUARTERLY = 0;
      TIMELINE_LIMIT_YEARLY = 0;
    };
  };
  # Both home directories are snapshotted; only administrators can browse copies.
  systemd.tmpfiles.rules = [ "d /home/.snapshots 0700 root root - -" ];
}
