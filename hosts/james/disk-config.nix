{ lib, ... }:
{
  disko.devices = {
    # Mount `/tmp` on `tmpfs`.
    #
    # NOTE: `tmpfs` doesn't reserve memory, so this can be liberal; as always,
    # there can be problems associated w/ over-provisioning memory so be careful
    # to balance these settings w/ ZRAM-swap (configured elsewhere).
    nodev."/tmp" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "size=4G"
        "mode=1777"
      ];
    };
    disk = {
      qemu = {
        type = "disk";
        device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_62732699";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "rpool";
              };
            };
          };
        };
      };
      tank = {
        type = "disk";
        device = "/dev/disk/by-id/scsi-0HC_Volume_102640374";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              device = "/dev/disk/by-id/scsi-0HC_Volume_102640374-part1";
              content = {
                type = "zfs";
                pool = "tank";
              };
            };
          };
        };
      };
    };
    zpool = {
      tank = {
        type = "zpool";
        rootFsOptions = {
          canmount = "off";
          mountpoint = "none";
          xattr = "sa";
          atime = "off";
          acltype = "posixacl";
          compression = "zstd";
          "com.sun:auto-snapshot" = "false";
        };
        options.ashift = "12";
        datasets = {
          # Static reservation so the pool will never be 100% full.
          #
          # If a pool fills up completely, delete this & reclaim space; don't
          # forget to re-create it afterwards!
          reservation = {
            type = "zfs_fs";
            options = {
              canmount = "off";
              mountpoint = "none";
              refreservation = "2G";
              primarycache = "none";
              secondarycache = "none";
            };
          };
        };
      };
      rpool = {
        type = "zpool";
        rootFsOptions = {
          canmount = "off";
          mountpoint = "none";
          xattr = "sa";
          atime = "off";
          acltype = "posixacl";
          compression = "zstd";
          "com.sun:auto-snapshot" = "false";
        };
        options.ashift = "12";
        postCreateHook = ''
          zfs snapshot rpool/local/nix@blank
          zfs snapshot rpool/local/root@blank
        '';
        datasets = {
          # Static reservation so the pool will never be 100% full.
          #
          # If a pool fills up completely, delete this & reclaim space; don't
          # forget to re-create it afterwards!
          reservation = {
            type = "zfs_fs";
            options = {
              canmount = "off";
              mountpoint = "none";
              refreservation = "2G";
              primarycache = "none";
              secondarycache = "none";
            };
          };
          "local/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options.mountpoint = "legacy";
          };
          "local/root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options.mountpoint = "legacy";
          };
          "safe/persist" = {
            type = "zfs_fs";
            mountpoint = "/persist";
            options = {
              "com.sun:auto-snapshot" = "false";
              mountpoint = "legacy";
            };
          };
          "safe/extra" = {
            type = "zfs_fs";
            mountpoint = "/persist/extra";
            options.mountpoint = "legacy";
          };
          "safe/extra/atuin" = {
            type = "zfs_fs";
            mountpoint = "/persist/extra/atuin";
            options = {
              sync = "disabled";
              "com.sun:auto-snapshot" = "false";
              mountpoint = "legacy";
            };
          };
          "safe/svc" = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
            };
          };
          "safe/svc/containers" = {
            type = "zfs_fs";
            options = {
              mountpoint = "/var/lib/containers";
              "com.sun:auto-snapshot" = "false";
            };
          };
        };
      };
    };
  };

  fileSystems."/nix".neededForBoot = true;
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/persist/extra".neededForBoot = true;
  fileSystems."/persist/extra/atuin".neededForBoot = true;
}
