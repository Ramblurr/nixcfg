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
#  - Boot encryption key stored on external USB disk
#  - Boot encryption key unlocks the 2nd partition which contains the encryption key for swap+zfs
#  - Fallback passphrase to unlock in case usb stick is gone
#  - ZFS datasets for /, /home, /persist (for use with impermanence)
#  - Root ZFS dataset is not encrypted so later you can add unencrypted datasets if you need to.
#  - SSH authorized key auto added
#
# You must edit the script and set a view variables. The script must also be executed as root.
#
# Example: `sudo ./disks.sh`
#

set -euo pipefail

DISK=vda # the disk to partition for nixos e.g, nvme0n1
DISK_IS_NVME=no # whether the disk is an nvme or not: yes or no
USB_DISK=vdb # should be usb stick for encryption key file
SWAPSIZE="4G"

################################################################################

export COLOR_RESET="\033[0m"
export RED_BG="\033[41m"
export BLUE_BG="\033[44m"
export YELLOW_BG="\033[43m"


function err {
    echo -e "${RED_BG}$1${COLOR_RESET}"
}

function info {
    echo -e "${BLUE_BG}$1${COLOR_RESET}"
}

function warning {
    echo -e "${YELLOW_BG}$1${COLOR_RESET}"
}

function prompt_danger {
    echo -e "⚠️ ⚠${YELLOW_BG}$1${COLOR_RESET} ⚠️⚠"
    read -p "Are you sure? (Type 'yes' in captial letters): " -r
    echo
    echo
    if ! [[ $REPLY == "YES" ]];
    then
        err "Aborting"
        exit 1
    fi
}

################################################################################


if [[ "$DISK" == "changeme" ]]; then
    err "You haven't configured the script. Please edit the script and set the variables."
    exit 1
fi

export DISK_PATH="/dev/${DISK}"
export USB_DISK_PATH="/dev/${USB_DISK}"

if ! [[ -b "$DISK_PATH" ]]; then
    err "Invalid argument: DISK='${DISK_PATH}' is not a block special file"
    exit 1
fi

if ! [[ -b "$USB_DISK_PATH" ]]; then
    err "Invalid argument: USB_DISK='${USB_DISK_PATH}' is not a block special file"
    exit 1
fi


if [[ "$EUID" -gt 0 ]]; then
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

################################################################################

prompt_danger "This will destroy all partitions and data on the root disk ${DISK_PATH} and USB disk ${USB_DISK_PATH} irrevoably."

info "Running the UEFI (GPT) partitioning for the root disk"

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
# name partitions
sgdisk -c 1:"boot" -c 2:"rawcryptkey" -c 3:"rawcryptswap" -c 4:"rawcryptrpool" "$DISK_PATH"


info "Running the UEFI (GPT) partitioning for the USB key"
blkdiscard -f "$USB_DISK_PATH"
sgdisk --zap-all "$USB_DISK_PATH"
sgdisk --clear  --mbrtogpt "$USB_DISK_PATH"
sgdisk -n 0:1M:+513M -t 0:8309 "$USB_DISK_PATH"
sgdisk -c 1:"usbbootkey" "$USB_DISK_PATH"

if [[ "$DISK_IS_NVME" = "yes" ]]; then
  export BOOT="${DISK_PATH}p1"
  export CRYPTKEY="${DISK_PATH}p2"
  export SWAP="${DISK_PATH}p3"
  export ZFS="${DISK_PATH}p4"
  export USB_BOOT_KEY="${USB_DISK_PATH}p1"
else
  export BOOT="${DISK_PATH}1"
  export CRYPTKEY="${DISK_PATH}2"
  export SWAP="${DISK_PATH}3"
  export ZFS="${DISK_PATH}4"
  export USB_BOOT_KEY="${USB_DISK_PATH}1"
fi

partprobe "$DISK_PATH"
sleep 1

info "Formatting boot partition $BOOT ..."
mkfs.vfat -n boot "$BOOT"

info "Formatting cryptkey partition $CRYPTKEY ..."
info "Adding fallback passphrase to slot 0"
printf "changeme" | cryptsetup luksFormat --batch-mode --label cryptkey "${CRYPTKEY}" -
printf "changeme" | cryptsetup open "${CRYPTKEY}" cryptkey -
info "Generating boot encryption key ..."
dd if=/dev/random of=boot.key bs=4096 count=1
info "Adding boot.key to usb disk ${USB_BOOT_KEY} as slot 1"
printf "changeme" | cryptsetup luksAddKey "${CRYPTKEY}" ./boot.key
dd if=./boot.key of="${USB_BOOT_KEY}"

info "Generating zfs encryption key ..."
echo "" > newline
dd if=/dev/zero bs=1 count=1 seek=1 of=newline
dd if=/dev/urandom bs=32 count=1 | od -A none -t x | tr -d '[:space:]' | cat - newline > hdd.key
dd if=hdd.key of=/dev/mapper/cryptkey
dd if=/dev/mapper/cryptkey bs=64 count=1

info "Encrypting swap partition $SWAP ..."
cryptsetup luksFormat --batch-mode --label cryptswap --key-file=/dev/mapper/cryptkey --keyfile-size=64 "$SWAP"
cryptsetup open --key-file=/dev/mapper/cryptkey --keyfile-size=64 "$SWAP" cryptswap

info "Enabling swap on /dev/mapper/cryptswap ..."
mkswap /dev/mapper/cryptswap
swapon /dev/mapper/cryptswap

info "Creating '$ZFS_POOL' ZFS pool for '$ZFS' ..."
# we do not encrypt the root pool, so we leave open the option
# to add other un-encrypted datasets later
zpool create \
    -o ashift=12 \
    -o autotrim=on \
    -O acltype=posixacl \
    -O compression=zstd \
    -O normalization=formD \
    -O xattr=sa \
    -O relatime=on \
    -f $ZFS_POOL $ZFS

info "Creating '$ZFS_ENCRYPTED' ZFS pool for '$ZFS' ..."
zfs create \
    -p \
    -o mountpoint=legacy \
    -o encryption=aes-256-gcm \
    -o keyformat=hex \
    -o keylocation=file:///dev/mapper/cryptkey \
    "$ZFS_DS_ROOT"

info "Creating '$ZFS_DS_ROOT' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_ROOT"

info "Configuring extended attributes setting for '$ZFS_DS_ROOT' ZFS dataset ..."
zfs set xattr=sa "$ZFS_DS_ROOT"

info "Configuring access control list setting for '$ZFS_DS_ROOT' ZFS dataset ..."
zfs set acltype=posixacl "$ZFS_DS_ROOT"

info "Creating '${ZFS_DS_ROOT}@blank' ZFS snapshot ..."
zfs snapshot "${ZFS_DS_ROOT}@blank"

info "Mounting '$ZFS_DS_ROOT' to /mnt ..."
mount -t zfs "$ZFS_DS_ROOT" /mnt

info "Mounting '$BOOT' to /mnt/boot ..."
mkdir -p /mnt/boot
mount -t vfat "$BOOT" /mnt/boot

info "Creating '$ZFS_DS_NIX' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_NIX"

info "Creating '${ZFS_DS_NIX}@blank' ZFS snapshot ..."
zfs snapshot "${ZFS_DS_NIX}@blank"

info "Disabling access time setting for '$ZFS_DS_NIX' ZFS dataset ..."
zfs set atime=off "$ZFS_DS_NIX"

info "Mounting '$ZFS_DS_NIX' to /mnt/nix ..."
mkdir -p /mnt/nix
mount -t zfs "$ZFS_DS_NIX" /mnt/nix

info "Creating '$ZFS_DS_HOME' ZFS dataset ..."
zfs create -p -o mountpoint=legacy "$ZFS_DS_HOME"

info "Creating '${ZFS_DS_HOME}@blank' ZFS snapshot ..."
zfs snapshot "${ZFS_DS_HOME}@blank"

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

echo
info "Time to change the fallback decryption passphrase."
info "The default passphrase is changeme"
info "When prompted for the passphrase to be changed enter changeme"
info "then provide your own passphrase."
echo
cryptsetup luksChangeKey "${CRYPTKEY}" --key-slot 0

info "Starting verification process."
echo
info "Umounting all"
touch /mnt/home/marker
mkdir /mnt/etc
umount /mnt/boot
umount /mnt/home
umount /mnt/persist
umount /mnt/nix
umount /mnt
zpool export rpool
swapoff /dev/mapper/cryptswap
sleep 1
cryptsetup close /dev/mapper/cryptswap
cryptsetup close /dev/mapper/cryptkey
sleep 1

info "Verifying your fallback passphrase"
cryptsetup open "${CRYPTKEY}" cryptkey
cryptsetup close /dev/mapper/cryptkey
info "Good job! Testing the key on the USB stick."
cryptsetup open --key-file="${USB_BOOT_KEY}" --keyfile-size 4096 "${CRYPTKEY}" cryptkey
cryptsetup open --key-file=/dev/mapper/cryptkey --keyfile-size=64 "$SWAP" cryptswap
info "Importing the zfs pool"
zpool import -l rpool
info "Mounting the fs"
mount -t zfs "$ZFS_DS_ROOT" /mnt
mount -t vfat "$BOOT" /mnt/boot
mount -t zfs "$ZFS_DS_NIX" /mnt/nix
mount -t zfs "$ZFS_DS_HOME" /mnt/home
mount -t zfs "$ZFS_DS_PERSIST" /mnt/persist
info "Checking if our marker is there.."

if [ ! -f "/mnt/home/marker" ]; then
  echo "Uhoh something went wrong, could not find marker file."
  exit 1
else
  rm /mnt/home/marker
fi

echo
echo
warning "⚠️  URGENT ⚠️ "
echo
info "Backup the $PWD/boot.key and $PWD/hdd.key file to a safe place, before you reboot."
info "Don't forget that your usb key (${USB_DISK_PATH}) is required for the system to boot unattended"
echo
warning "⚠️  URGENT ⚠️ "
echo
echo

info "Done!"

echo


info "Now proceed with run install.sh"
