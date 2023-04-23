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


USER_NAME=ramblurr
INITIAL_PASSWORD=changeme
AUTHORIZED_SSH_KEY="ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== smartcard"

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

function prompt_danger {
    echo -e "⚠ ${YELLOW_BG}$1${COLOR_RESET}⚠"
    read -p "Are you sure? (Type 'yes' in captial letters): " -r
    echo
    echo
    if ! [[ $REPLY == "YES" ]];
    then
        err "Aborting"
        exit 1
    fi
}

function check_key {
    if [[ -f "/mnt/persist/etc/ssh/ssh_host_ed25519_key" ]];
    then
        echo
        info "Here we go..."
    else
        mkdir -p /mnt/persist/etc/ssh
        echo
        echo "Please create /mnt/persist/etc/ssh/ssh_host_ed25519_key"
        echo "It is used to decrypt secrets."
        echo
        read -p "Are you ready? (type 'yes' in capital letters to begin installation): " -r
        if ! [[ $REPLY == "YES" ]];
        then
            err "Aborting"
            exit 1
        fi
        check_key
    fi
}

################################################################################

if [[ "$EUID" -gt 0 ]]; then
    err "Must run as root"
    exit 1
fi

if [[ ! -d /mnt/boot ]]; then
    err "No /mnt/boot directory found. Did you run disks.sh first?"
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

info "Creating persistent directories ..."

mkdir -p /mnt/persist/etc/ssh
mkdir -p /persist/var/lib/bluetooth
mkdir -p /persist/etc/ssh

check_key

chown -R root:root /mnt/persist/etc/ssh/ssh_host_ed25519_key
chmod 0600 /mnt/persist/etc/ssh/ssh_host_ed25519_key

info "Generating NixOS configuration (/mnt/etc/nixos/*.nix) ..."
nixos-generate-config --root /mnt


info "Enter password for '${USER_NAME}' user ..."
USER_PASSWORD_HASH="$(printf "${INITIAL_PASSWORD}" | mkpasswd -m sha-512 --stdin | sed 's/\$/\\$/g')"

info "Moving generated hardware-configuration.nix to /persist/etc/nixos/ ..."
mkdir -p /mnt/persist/etc/nixos
mv /mnt/etc/nixos/hardware-configuration.nix /mnt/persist/etc/nixos/hardware-configuration.nix.original
cp /home/nixos/nixcfg/hosts/quine/hardware-configuration.nix /mnt/persist/etc/nixos/


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


  nix.nixPath =
    [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=/persist/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];

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
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
    hostKeys =
      [
        {
          path = "/persist/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
  };
  nix.package = pkgs.nixFlakes;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  system.stateVersion = "23.05";

}
EOF

info "Installing NixOS to /mnt ..."
ln -s /mnt/persist/etc/nixos/configuration.nix /mnt/etc/nixos/configuration.nix
ln -s /mnt/persist/etc/nixos/hardware-configuration.nix /mnt/etc/nixos/hardware-configuration.nix
nixos-install -I "nixos-config=/mnt/persist/etc/nixos/configuration.nix" --no-root-passwd  # already prompted for and configured password
