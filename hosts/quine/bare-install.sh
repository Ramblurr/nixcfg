#!/usr/bin/env bash

#
# NixOS install script synthesized from:
#
#   - https://gist.github.com/mx00s/ea2462a3fe6fdaa65692fe7ee824de3e
#   - Erase Your Darlings (https://grahamc.com/blog/erase-your-darlings)
#   - OpenZFS NixOS manual (https://openzfs.github.io/openzfs-docs/Getting%20Started/NixOS/Root%20on%20ZFS/0-overview.html)
#   - ZFS Datasets for NixOS (https://grahamc.com/blog/nixos-on-zfs)
#   - NixOS Manual (https://nixos.org/nixos/manual/)
#   - https://gist.github.com/bobberb/025043a3897ab149a268fff1dd0c0082
#   - https://github.com/dr460nf1r3/dr460nixed/blob/d958facca7d2efd3fcf7803ecc372f56b84b7042/hosts/slim-lair/disks.sh#L11
#   - https://gist.github.com/lucasvo/35e0745b72dd384dcb9b9ee5bae5fecb
#
#
# Features:
#  - UEFI (GPT) partitioning
#  - Un-encrypted /boot
#  - Encrypted SWAP
#  - Encrypted ZFS root (using native zfs encryption)
#  - Single passphrase entry to unlock (actual encryption key is stored in the 2nd partition)
#  - ZFS datasets for /, /home, /persist (for use with impermanence)
#  - SSH authorized key auto added
#
# You must edit the script and set a view variables. The script must also be executed as root.
#
# Example: `sudo ./install.sh`
#

set -euo pipefail


DISK=changeme # e.g, nvme0n1
USER_NAME=ramblurr
INITIAL_PASSWORD=changeme
SWAPSIZE="72G"
AUTHORIZED_SSH_KEY="ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== smartcard"

################################################################################

export COLOR_RESET="\033[0m"
export RED_BG="\033[41m"
export BLUE_BG="\033[44m"

function err {
    echo -e "${RED_BG}$1${COLOR_RESET}"
}

function info {
    echo -e "${BLUE_BG}$1${COLOR_RESET}"
}

################################################################################


if ! [[ "$DISK" == "changeme" ]]; then
    err "You haven't configured the script. Please edit the script and set the variables."
    exit 1
fi

export DISK_PATH="/dev/${DISK}"

if ! [[ -b "$DISK_PATH" ]]; then
    err "Invalid argument: '${DISK_PATH}' is not a block special file"
    exit 1
fi


if [[ "$EUID" > 0 ]]; then
    err "Must run as root"
    exit 1
fi

export ZFS_POOL="rpool"

# ephemeral datasets
export ZFS_ENCRYPTED="${ZFS_POOL}/encrypted"
export ZFS_LOCAL="${ZFS_ENCRYPTED}/local"
export ZFS_DS_ROOT="${ZFS_LOCAL}/root"
export ZFS_DS_NIX="${ZFS_LOCAL}/nix"

# persistent datasets
export ZFS_SAFE="${ZFS_ENCRYPTED}/safe"
export ZFS_DS_HOME="${ZFS_SAFE}/home"
export ZFS_DS_PERSIST="${ZFS_SAFE}/persist"

export ZFS_BLANK_SNAPSHOT="${ZFS_DS_ROOT}@blank"

################################################################################

info "Running the UEFI (GPT) partitioning"

blkdiscard -f "$DISK_PATH"
sgdisk --zap-all "$DISK_PATH"
sgdisk --clear  --mbrtogpt "$DISK_PATH"

# boot
sgdisk -n 0:1M:+513M -t 0:EF00 "$DISK_PATH"
# cryptkey - stores the encryption key
sgdisk -n 0:0:+64M -t 0:8309 "$DISK_PATH"
# swap
sgdisk -n 0:0:+${SWAPSIZE} -t 0:8200 "$DISK_PATH"
# zfs
sgdisk -n 0:0:0 -t 0:EF00 "$DISK_PATH"

export BOOT="${DISK_PATH}p1"
export CRYPTKEY="${DISK_PATH}p2"
export SWAP="${DISK_PATH}p3"
export ZFS="${DISK_PATH}p4"

partprobe "$DISK_PATH"
sleep 1

BOOT_DISK_UUID="$(blkid --match-tag UUID --output value $BOOT)"
SWAP_DISK_PARTUUID="$(blkid --match-tag PARTUUID --output value $SWAP)"
CRYPTKEY_DISK_PARTUUID="$(blkid --match-tag PARTUUID --output value $CRYPTKEY)"

info "Formatting boot & swap partition ..."
mkfs.vfat -n boot "$BOOT"

info "Formatting cryptkey partition"
echo "changeme" | cryptsetup luksFormat --label cryptkey "${CRYPTKEY}" -
echo "changeme" | cryptsetup open "${CRYPTKEY}" cryptkey -

info "Generating encryption key"
echo "" > newline
dd if=/dev/zero bs=1 count=1 seek=1 of=newline
dd if=/dev/urandom bs=32 count=1 | od -A none -t x | tr -d '[:space:]' | cat - newline > hdd.key
dd if=hdd.key of=/dev/mapper/cryptkey
dd if=/dev/mapper/cryptkey bs=64 count=1

info "Formatting encrypted swap partition ..."
echo "changeme" | cryptsetup luksFormat --label cryptswap --key-file=/dev/mapper/cryptkey --keyfile-size=64 "" -
echo "changeme" | cryptsetup open --key-file=/dev/mapper/cryptkey --keyfile-size=64 "$SWAP" cryptswap -

mkswap /dev/mapper/cryptswap
swapon /dev/mapper/cryptswap

info "Creating '$ZFS_POOL' ZFS pool for '$ZFS' ..."
# we do not encrypt the root pool, so we leave open the option
# to add other un-encrypted datasets later
zpool create \
    -O acltype=posixacl \
    -O compression=zstd \
    -O normalization=formD \
    -O xattr=sa \
    -f $ZFS_POOL $ZFS

zpool set autotrim=on $ZFS_POOL

ZFS_DISK_UUID="$(blkid --match-tag UUID --output value $ZFS)"

info "Creating '$ZFS_ENCRYPTED' ZFS pool for '$ZFS' ..."
zfs create \
    -p \
    -o mountpoint=legacy \
    -o encryption=aes-256-gcm \
    -O keyformat=hex \
    -O keylocation=file:///dev/mapper/cryptkey \
    "$ZFS_DS_ROOT"

info "Creating '$ZFS_DS_ROOT' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_ROOT"

info "Configuring extended attributes setting for '$ZFS_DS_ROOT' ZFS dataset ..."
zfs set xattr=sa "$ZFS_DS_ROOT"

info "Configuring access control list setting for '$ZFS_DS_ROOT' ZFS dataset ..."
zfs set acltype=posixacl "$ZFS_DS_ROOT"

info "Creating '$ZFS_BLANK_SNAPSHOT' ZFS snapshot ..."
zfs snapshot "$ZFS_BLANK_SNAPSHOT"

info "Mounting '$ZFS_DS_ROOT' to /mnt ..."
mount -t zfs "$ZFS_DS_ROOT" /mnt

info "Mounting '$BOOT' to /mnt/boot ..."
mkdir -p /mnt/boot
mount -t vfat "$BOOT" /mnt/boot

info "Creating '$ZFS_DS_NIX' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_NIX"

info "Disabling access time setting for '$ZFS_DS_NIX' ZFS dataset ..."
zfs set atime=off "$ZFS_DS_NIX"

info "Mounting '$ZFS_DS_NIX' to /mnt/nix ..."
mkdir -p /mnt/nix
mount -t zfs "$ZFS_DS_NIX" /mnt/nix

info "Creating '$ZFS_DS_HOME' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_HOME"

info "Mounting '$ZFS_DS_HOME' to /mnt/home ..."
mkdir -p /mnt/home
mount -t zfs "$ZFS_DS_HOME" /mnt/home

info "Creating '$ZFS_DS_PERSIST' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_PERSIST"

info "Mounting '$ZFS_DS_PERSIST' to /mnt/persist ..."
mkdir -p /mnt/persist
mount -t zfs "$ZFS_DS_PERSIST" /mnt/persist

info "Permit ZFS auto-snapshots on ${ZFS_SAFE}/* datasets ..."
zfs set com.sun:auto-snapshot=true "$ZFS_DS_HOME"
zfs set com.sun:auto-snapshot=true "$ZFS_DS_PERSIST"

info "Creating persistent directories ..."
mkdir -p /mnt/persist/etc/ssh
mkdir -p /persist/etc/NetworkManager/system-connections
mkdir -p /persist/var/lib/bluetooth
mkdir -p /persist/etc/ssh

info "Generating NixOS configuration (/mnt/etc/nixos/*.nix) ..."
nixos-generate-config --root /mnt


info "Enter password for '${USER_NAME}' user ..."
USER_PASSWORD_HASH="$(printf "${INITIAL_PASSWORD}" | mkpasswd -m sha-512 --stdin | sed 's/\$/\\$/g')"

info "Moving generated hardware-configuration.nix to /persist/etc/nixos/ ..."
mkdir -p /mnt/persist/etc/nixos
mv /mnt/etc/nixos/hardware-configuration.nix /mnt/persist/etc/nixos/

info "Backing up the originally generated configuration.nix to /persist/etc/nixos/configuration.nix.original ..."
mv /mnt/etc/nixos/configuration.nix /mnt/persist/etc/nixos/configuration.nix.original

info "Writing NixOS configuration to /persist/etc/nixos/ ..."
cat <<EOF > /mnt/persist/etc/nixos/configuration.nix
{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  fileSystems."/" =
    { device = "rpool/encrypted/local/root";
      fsType = "zfs";
      options= [ "zfsutil" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/${BOOT_DISK_UUID}";
      fsType = "vfat";
    };

  fileSystems."/nix" =
    { device = "rpool/encrypted/local/nix";
      fsType = "zfs";
      options= ["zfsutil"];
    };

  fileSystems."/home" =
    { device = "rpool/encrypted/safe/home";
      fsType = "zfs";
      options= ["zfsutil"];
    };

  fileSystems."/persist" =
    { device = "rpool/encrypted/safe/persist";
      fsType = "zfs";
      options= ["zfsutil"];
    };

  swapDevices = [
    { device = "/dev/disk/by-partuuid/${SWAP_DISK_PARTUUID}"; randomEncryption.enable = true;}
  ];

  nix.nixPath =
    [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=/persist/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.devNodes="${DISK_PATH}";

  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.kernelParams = [ "elevator=none" ];
  boot.initrd.availableKernelModules = ["aesni_intel" "cryptd"];
  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-label/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-label/cryptswap";
      keyFile = "/dev/mapper/cryptkey";
      keyFileSize = 64;
    };
  };
  boot.initrd.postMountCommands = ''
    # Don't keep the cryptkey available all the time.
    cryptsetup close /dev/mapper/cryptkey
  '';
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r ${ZFS_BLANK_SNAPSHOT}
  '';

  networking = {
    hostId = "$(head -c 8 /etc/machine-id)";
    useDHCP = true;
  };
  environment.etc."machine-id".text = "$(cat /etc/machine-id)";

  services.zfs = {
    autoScrub.enable = true;
    autoSnapshot.enable = true;
    # TODO: autoReplication
  };
  users = {
    mutableUsers = false;
    users = {
      ${USER_NAME} = {
        initialHashedPassword = "${USER_PASSWORD_HASH}";
        extraGroups = [ "wheel" ];
        isNormalUser = true;
        uid = 1000;
        home = "/home/${USER_NAME}";
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [ "${AUTHORIZED_SSH_KEY}" ];
      };
    };
  };

  environment.systemPackages = with pkgs;
    [
      vim
      curl
      ripgrep
      git
      wget
    ];

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    hostKeys =
      [
        {
          path = "/persist/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
  };

}
EOF

info "Installing NixOS to /mnt ..."
ln -s /mnt/persist/etc/nixos/configuration.nix /mnt/etc/nixos/configuration.nix
nixos-install -I "nixos-config=/mnt/persist/etc/nixos/configuration.nix" --no-root-passwd  # already prompted for and configured password
