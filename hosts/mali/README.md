# Mali - a NAS

## Current boot and storage notes

Last verified after wiping the retired Crucial disk: generation 214 on 2026-06-10.

Final boot path:

- `rpool2/encrypted` is unlocked with ZFS native encryption using `keyformat=hex` and `keylocation=file:///run/cryptkey/keyfile`.
- The initrd opens LUKS `cryptkey` from `/dev/disk/by-partlabel/cryptkey` with the USB key at `/dev/disk/by-partlabel/usbbootkey`.
- The unlocked `/dev/mapper/cryptkey` is an ext4 filesystem labeled `cryptkeyfs` and is mounted read-only at `/run/cryptkey`.
- `/run/cryptkey/keyfile` is the 64-byte ZFS key file used by `rpool2/encrypted`.
- The configured swap source is the Samsung disk LUKS UUID `cc96454e-8d13-43bb-8c97-a199dcece4ca`; initrd unlocks it as `/dev/mapper/cryptswap` via the keyfile-on-device path `/keyfile:/dev/mapper/cryptkey`.
- Old generations that expect raw key material directly at `/dev/mapper/cryptkey` are not safe rollback targets without manual recovery steps.

Current observed device layout:

| Purpose | Stable path | Current backing device | Label | Partlabel | UUID | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| Final cryptkey | `/dev/disk/by-partlabel/cryptkey` | `/dev/sdl2` | `cryptkey` | `cryptkey` | `b4158773-1988-47c9-8aec-70adfa56b7cb` | Boot-critical LUKS container for `/run/cryptkey`. |
| USB boot key | `/dev/disk/by-partlabel/usbbootkey` | `/dev/sdo1` | none | `usbbootkey` | none | Used to unlock final `cryptkey`. |
| Configured swap | `/dev/disk/by-uuid/cc96454e-8d13-43bb-8c97-a199dcece4ca` | `/dev/sdl3` | `cryptswap2` | `Linux swap` | `cc96454e-8d13-43bb-8c97-a199dcece4ca` | Unlocked as `/dev/mapper/cryptswap`; lives on the Samsung OS disk. |
| Retired old OS disk | `/dev/disk/by-id/ata-Crucial_CT525MX300SSD1_171816E66F09` | `/dev/sdm` | none | none | none | Partition table and old boot/cryptkey/cryptswap/rpool signatures wiped; no active users. |

Do not use old generations that expect raw key material directly at `/dev/mapper/cryptkey` or the old Crucial-disk swap layout without manual recovery.

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
