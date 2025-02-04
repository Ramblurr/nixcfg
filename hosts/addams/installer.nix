{
  config,
  pkgs,
  lib,
  modulesPath,
  targetSystem,
  ...
}:
let
  installer = pkgs.writeShellApplication {
    name = "installer";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      dosfstools
      e2fsprogs
      gawk
      nixos-install-tools
      util-linux
      config.nix.package
      zfs
      gitMinimal
      gnugrep
      findutils
    ];
    text = ''
      set -euo pipefail
      KEYS_DISK=""
      detect_and_mount_bootstrap() {
        local mount_point="/bootstrap"

        if mountpoint -q "$mount_point"; then
          echo "Bootstrap directory already mounted (likely VM shared folder)"
          return 0
        fi

       local device attempts=0
       echo "Please insert the USB key labeled 'NIXKEYS'..."
       while [ $attempts -lt 120 ]; do  # 10 minutes
         for i in $(blkid -o device); do
           if blkid "$i" | grep -q 'LABEL="NIXKEYS"'; then
             device="$i"
             break 2
           fi
         done
         echo -n "."
         sleep 5
         attempts=$((attempts + 1))
       done
       echo  # new line after dots

        if [ -z "$device" ]; then
          echo "ERROR: Could not find USB device with label NIXKEYS"
          exit 1
        fi

        echo "Found bootstrap USB device at $device"

        mkdir -p "$mount_point"

        set +e
        if ! mount -t vfat "$device" "$mount_point"; then
          echo "ERROR: Failed to mount bootstrap device"
          return 1
        fi
        set -e

        KEYS_DISK="$device"
        echo "Successfully mounted bootstrap device at $mount_point"
        return 0
      }

      echo "STARTING..."
      echo "Unattended installation script for ${targetSystem.config.system.build.toplevel}"
      echo

      echo "Setting up bootstrap keys..."
      if ! detect_and_mount_bootstrap; then
        echo "ERROR: Failed to setup bootstrap keys!"
        exit 1
      fi

      if [ ! -f "/bootstrap/ssh_host_ed25519_key" ] || [ ! -f "/bootstrap/ssh_host_ed25519_key.pub" ]; then
        echo "ERROR: Required SSH key files ssh_host_ed25519_key and ssh_host_ed25519_key.pub not found in bootstrap media!"
        exit 1
      fi
       echo "Searching for disk to use..."
       for i in $(lsblk -pln -o NAME,TYPE | grep disk | awk '{ print $1 }'); do
         if [[ "$i" == "/dev/fd0" ]]; then
           echo "DISK: $i is a floppy, skipping..."
           continue
         fi
         if grep -ql "^$i" <(mount); then
           echo "DISK: $i is in use, skipping..."
         else
           DEVICE_MAIN="$i"
           break
         fi
       done
       if [[ -z "$DEVICE_MAIN" ]]; then
         echo "ERROR: No usable disk found on this machine!"
         exit 1
       else
         echo "DISK: Found $DEVICE_MAIN"
      fi

      echo
      echo
      echo Ready to install!
      echo
      echo "Install Disk: $DEVICE_MAIN (will be erased\!)"
      echo "Keys Disk: $KEYS_DISK"
      echo
      echo

      sleep 30http://10.9.4.25:8123/

      echo "Wiping and partioning disk..."

      DISKO_DEVICE_MAIN=''${DEVICE_MAIN#"/dev/"} ${targetSystem.config.system.build.diskoScript}

      install -d -m755 "/mnt/persist/etc/ssh"
      install -d -m700 "/mnt/persist/root/"
      install -d -m700 "/mnt/persist/root/.ssh"
      install -d -m755 "/mnt/persist/home"

      cp /bootstrap/ssh_host_ed25519_key "/mnt/persist/etc/ssh/ssh_host_ed25519_key"
      cp /bootstrap/ssh_host_ed25519_key.pub "/mnt/persist/etc/ssh/ssh_host_ed25519_key.pub"
      chmod 600 "/mnt/persist/etc/ssh/ssh_host_ed25519_key"

      umount /bootstrap

      sleep 10

      echo "Installing the system..."
      nixos-install --no-channel-copy --no-root-password --option substituters "" --system ${targetSystem.config.system.build.toplevel}

      echo "Cleaning up...."
      umount -Rv /mnt/
      swapoff -a
      zpool export -a || true

      echo "Done! Rebooting..."
      nohup sh -c 'sleep 6 && reboot' >/dev/null &
    '';
  };
  installerFailsafe = pkgs.writeShellScript "failsafe" ''
    ${lib.getExe installer} || echo "ERROR: Installation failure!"
    sleep 3600
  '';
in
{
  imports = [
    (modulesPath + "/installer/cd-dvd/iso-image.nix")
    (modulesPath + "/profiles/all-hardware.nix")
  ];

  boot.kernelParams = [ "systemd.unit=getty.target" ];
  boot.supportedFilesystems = [
    "btrfs"
    "cifs"
    "f2fs"
    "ntfs"
    "vfat"
    "xfs"
    "zfs"
  ];
  networking.hostId = lib.mkDefault "8425e349";

  console = {
    earlySetup = true;
    font = "ter-v16n";
    packages = [ pkgs.terminus_font ];
  };

  isoImage.isoName = "${config.isoImage.isoBaseName}-${config.system.nixos.label}-${pkgs.stdenv.hostPlatform.system}.iso";
  isoImage.makeEfiBootable = true;
  isoImage.makeUsbBootable = true;
  isoImage.squashfsCompression = "zstd -Xcompression-level 15"; # xz takes forever

  #fileSystems."/bootstrap" = {
  #  device = "host0";
  #  fsType = "9p";
  #  options = [
  #    "trans=virtio"
  #    "version=9p2000.L"
  #    "msize=104857600"
  #    "nofail"
  #  ];
  #};

  systemd.services."getty@tty1" = {
    overrideStrategy = "asDropin";
    serviceConfig = {
      ExecStart = [
        ""
        installerFailsafe
      ];
      Restart = "no";
      StandardInput = "null";
    };
  };

  system.stateVersion = "24.11";
}
