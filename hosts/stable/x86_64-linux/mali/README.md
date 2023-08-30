# Mali - a NAS

## Flake Install

1. Boot into installer

2. Follow:
  ``` sh
  # 1.
  # from workstation
  ssh-copy-id -f -i ~/.ssh/casey.pub nixos
  rsync  -avr ~/nixcfg/hosts/ nixos:nixcfg/hosts

  # 2.
  # from target
  cd /home/nixos/nixcfg/hosts/stable/x86_64-linux/mali
  sudo ./disks.sh

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
