# Quine

## Install

1. Boot into installer

2. Follow:
  ``` sh
  # go into a root shell
  sudo su

  # go inside a shell with properly required programs
  nix-shell -p git nixUnstable vim gptfdisk

  # create this folder if necessary
  mkdir -p /mnt/etc/

  git clone https://github.com/ramblurr/nixcfg.git /mnt/etc/nixos --recurse-submodules

  cd /mnt/etc/nixos/hosts/quine

  # edit disks.sh and verify vars

  # partition disk
  ./disks.sh

  # Edit /persist/etc/ssh/ssh_host_ed25519_key and place the private key
  # it is used to decrypt the secrets with sops

  # install
  ./install.sh

  ```

  You could get some hashes errors, just change the bad hashes in the config file
  to the given ones by the Nix Output.
3. Reboot
4. Login
5. `chown -R $USER /etc/nixos`
