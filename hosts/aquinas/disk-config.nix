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
        "size=16G"
        "mode=1777"
      ];
    };
    disk = {
      samsung960Evo = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-Samsung_SSD_960_EVO_1TB_S3ETNB0J413109E";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              priority = 1;
              name = "boot";
              size = "1M";
              type = "EF02";
            };
            esp = {
              priority = 2;
              name = "ESP";
              size = "500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [
                  "defaults"
                  "umask=0077"
                ];
              };
            };
            swap = {
              priority = 3;
              size = "34G";
              content = {
                type = "swap";
                randomEncryption = true;
                resumeDevice = true; # resume from hiberation from this device
              };
            };
            zfs = {
              priority = 4;
              size = "100%";
              content = {
                type = "zfs";
                pool = "rpool";
              };
            };
          };
        };
      };
    };
    zpool = {
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
          zfs snapshot rpool/encrypted/local/nix@blank
          zfs snapshot rpool/encrypted/local/root@blank
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
          encrypted = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
              canmount = "off";
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "prompt";
            };
            # use this to read the key during boot
            #postCreateHook = ''
            #  zfs set keylocation="prompt" "rpool/$name";
            #'';
          };
          "encrypted/local/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options.mountpoint = "legacy";
          };
          "encrypted/local/root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options.mountpoint = "legacy";
          };
          "encrypted/safe/persist" = {
            type = "zfs_fs";
            mountpoint = "/persist";
            options = {
              "com.sun:auto-snapshot" = "false";
              mountpoint = "legacy";
            };
          };
          "encrypted/safe/vms" = {
            type = "zfs_fs";
          };
          "encrypted/safe/extra" = {
            type = "zfs_fs";
            mountpoint = "/persist/extra";
            options.mountpoint = "legacy";
          };
          "encrypted/safe/extra/atuin" = {
            type = "zfs_fs";
            mountpoint = "/persist/extra/atuin";
            options = {
              sync = "disabled";
              "com.sun:auto-snapshot" = "false";
              mountpoint = "legacy";
            };
          };
          "encrypted/safe/svc" = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
            };
          };
          "encrypted/safe/svc/containers" = {
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
