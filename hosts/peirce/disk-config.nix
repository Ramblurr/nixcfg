{ lib, pkgs, ... }:
let
  mirrorDisks = {
    crucial = "/dev/disk/by-id/ata-CT1000MX500SSD4_1831E14C024D";
    samsung = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_1TB_S4CSNF0M979999W";
  };
in
{
  disko.devices = {
    nodev."/tmp" = {
      fsType = "tmpfs";
      mountOptions = [
        "defaults"
        "size=16G"
        "mode=1777"
      ];
    };
    # The OCZ Agility 4 is deliberately excluded from the pool.
    disk = lib.mapAttrs (name: device: {
      type = "disk";
      inherit device;
      content = {
        type = "gpt";
        partitions = {
          esp = {
            priority = 1;
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot/${name}";
              # A missing mirror member must not prevent degraded boot.
              mountOptions = [
                "defaults"
                "umask=0077"
                "nofail"
                "x-systemd.device-timeout=5s"
              ];
            };
          };
          cryptkey = {
            priority = 2;
            size = "1M";
            type = "6e4fc1ba-8431-4f25-96d0-588c91919f64";
            label = "peirce-cryptkey-${name}";
          };
          zfs = {
            priority = 3;
            size = "100%";
            content = {
              type = "zfs";
              pool = "rpool";
            };
          };
        };
      };
    }) mirrorDisks;
    zpool.rpool = {
      type = "zpool";
      mode = "mirror";
      options.ashift = "12";
      rootFsOptions = {
        canmount = "off";
        mountpoint = "none";
        xattr = "sa";
        atime = "off";
        acltype = "posixacl";
        compression = "zstd";
        "com.sun:auto-snapshot" = "false";
      };
      # These plaintext key copies allow unattended boot from either SSD.
      # They do not protect against theft of the disks or the whole machine.
      preCreateHook = ''
        (
          set -eu
          umask 077
          key=/run/peirce-zfs.key
          first=/dev/disk/by-partlabel/peirce-cryptkey-crucial
          second=/dev/disk/by-partlabel/peirce-cryptkey-samsung
          # Reuse an existing key; never overwrite it on a repeated invocation.
          if head -c 65 "$first" | grep -Eq '^[0-9a-f]{64}$'; then
            head -c 65 "$first" > "$key"
            head -c 65 "$second" | ${pkgs.diffutils}/bin/cmp - "$key"
          else
            # Refuse to replace keys if either disk already contains a ZFS member.
            for disk in /dev/disk/by-partlabel/disk-{crucial,samsung}-zfs; do
              if blkid -p -s TYPE -o value "$disk" | grep -qx zfs_member; then
                echo "Existing ZFS member without a usable key; refusing key generation" >&2
                exit 1
              fi
            done
            { od -An -N32 -tx1 /dev/urandom | tr -d ' \n'; printf '\n'; } > "$key"
            dd if="$key" of="$first" conv=fsync
            dd if="$key" of="$second" conv=fsync
          fi
        )
      '';
      postCreateHook = ''
        for dataset in local/nix local/root vms; do
          zfs list -t snapshot "rpool/encrypted/$dataset@blank" >/dev/null 2>&1 || \
            zfs snapshot "rpool/encrypted/$dataset@blank"
        done
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
            canmount = "off";
            mountpoint = "none";
            encryption = "aes-256-gcm";
            keyformat = "hex";
            keylocation = "file:///run/peirce-zfs.key";
          };
        };
        "encrypted/local/root" = {
          type = "zfs_fs";
          mountpoint = "/";
          options.mountpoint = "legacy";
        };
        "encrypted/local/nix" = {
          type = "zfs_fs";
          mountpoint = "/nix";
          options.mountpoint = "legacy";
        };
        "encrypted/vms" = {
          type = "zfs_fs";
          options.mountpoint = "none";
        };
        "encrypted/safe/persist" = {
          type = "zfs_fs";
          mountpoint = "/persist";
          options.mountpoint = "legacy";
        };
        "encrypted/safe/extra" = {
          type = "zfs_fs";
          mountpoint = "/persist/extra";
          options.mountpoint = "legacy";
        };
        "encrypted/safe/vms" = {
          type = "zfs_fs";
          options.mountpoint = "none";
        };
        # Service modules create their own child datasets as services are enabled.
        "encrypted/safe/svc" = {
          type = "zfs_fs";
          options.mountpoint = "none";
        };
      };
    };
  };

  fileSystems."/nix".neededForBoot = true;
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/persist/extra".neededForBoot = true;
}
