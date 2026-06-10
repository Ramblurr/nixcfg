# Mali - a NAS

## Current boot and storage notes

- `rpool2/encrypted` is unlocked with ZFS native encryption using `keyformat=hex` and `keylocation=file:///run/cryptkey/keyfile`.
- The initrd opens LUKS `cryptkey` from `/dev/disk/by-partlabel/cryptkey` with the USB key at `/dev/disk/by-partlabel/usbbootkey`, then mounts `/dev/mapper/cryptkey` read-only as ext4 at `/run/cryptkey`.
- Do not use `/dev/disk/by-label/cryptkey` for boot-critical config or operational commands; an old raw `cryptkey` partition may still provide that duplicate label.
- The configured swap target remains `/dev/disk/by-label/cryptswap`; initrd unlocks it via the keyfile-on-device path `/keyfile:/dev/mapper/cryptkey`.
- Old generations that expect raw key material directly at `/dev/mapper/cryptkey` are not safe rollback targets without manual recovery steps.

## Flake Install

1. Boot into installer

2. Follow:
  ``` sh
  # 1.
  # from workstation
  ssh-copy-id -f -i ~/.ssh/casey.pub nixos
  rsync  -avr ~/nixcfg nixos:

  # WARNING: ensure target disks are correct
  lsblk -a -o NAME,SIZE,VENDOR,NAME,MODEL,PARTLABEL

  # 2.
  # from target
  cd /home/nixos/nixcfg/hosts/stable/x86_64-linux/mali
  sudo ./disks.sh

  # 3. Save hdd.key and boot.key
  scp nixos:/home/nixos/nixcfg/hosts/stable/x86_64-linux/mali/boot.key .
  scp nixos:/home/nixos/nixcfg/hosts/stable/x86_64-linux/mali/hdd.key .
  # Add them to your password manager

  # 3.
  # go into a root shell
  sudo -i
  nix-shell -p git htop

  # 4. Resolve TODO(qemu) in hardware-configuration.nix

  # 5.
  bash ./install.sh

  # 6.
  # when prompted
  # from workstation
  # sops -d --extract "['ssh_host_ed25519_key']" ./secrets.sops.yaml | ssh nixos 'cat - | sudo tee -a /mnt/persist/etc/ssh/ssh_host_ed25519_key'
  # then on target type YES
  ```

3. Reboot
4. Login
