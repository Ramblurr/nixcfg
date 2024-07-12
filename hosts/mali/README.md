# Mali - a NAS

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
