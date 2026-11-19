_: {
  disko.devices = {
    nodev."/tmp" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "size=16G"
        "mode=1777"
      ];
    };

    disk = {
      bulk = {
        # SATA 2 TB Crucial MX500
        type = "disk";
        device = "/dev/disk/by-id/ata-CT2000MX500SSD1_2308E6B0D040";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "tank";
              };
            };
          };
        };
      };

      system = {
        # NVMe 2 TB Samsung 970 EVO Plus
        type = "disk";
        device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_2TB_S6P1NX0TA10916P";
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
            cryptkey = {
              priority = 3;
              size = "1K";
              type = "6e4fc1ba-8431-4f25-96d0-588c91919f64";
              name = "cryptkey";
              label = "cryptkey";
            };
            zfs = {
              priority = 4;
              size = "100%";
              content = {
                preCreateHook = ''
                  echo "" > newline
                  dd if=/dev/zero bs=1 count=1 seek=1 of=newline
                  dd if=/dev/urandom bs=32 count=1 | od -A none -t x | tr -d '[:space:]' | cat - newline > /tmp/hdd.key
                  dd if=/dev/zero bs=1024 count=1 of=/dev/disk/by-partlabel/cryptkey
                  dd if=/tmp/hdd.key of=/dev/disk/by-partlabel/cryptkey
                '';

                type = "zfs";
                pool = "rpool";
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
              keyformat = "hex";
              keylocation = "file:///dev/disk/by-partlabel/cryptkey";
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
          zfs snapshot rpool/encrypted/local/nix@blank
          zfs snapshot rpool/encrypted/local/root@blank
        '';
        datasets = {
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
              keyformat = "hex";
              keylocation = "file:///dev/disk/by-partlabel/cryptkey";
            };
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
          "encrypted/safe/extra" = {
            type = "zfs_fs";
            mountpoint = "/persist/extra";
            options.mountpoint = "legacy";
          };
          "encrypted/safe/microvms" = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
              "com.sun:auto-snapshot" = "false";
            };
          };
          "encrypted/safe/svc" = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
        };
      };
    };
  };

  fileSystems."/nix".neededForBoot = true;
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/persist/extra".neededForBoot = true;
}
