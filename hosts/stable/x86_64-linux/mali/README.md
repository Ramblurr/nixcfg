# Mali - a NAS

## Bare Install

1. Boot into installer

2. Follow:
  ``` sh
  # go into a root shell
  sudo su

  # create this folder if necessary
  mkdir -p /mnt/etc/

  git clone https://github.com/ramblurr/nixcfg.git /mnt/etc/nixos --recurse-submodules

  cd /mnt/etc/nixos/hosts/stable/x86_64-linux/mali

  # edit disks.sh and verify vars

  # partition disk
  ./disks.sh

  # Edit /persist/etc/ssh/ssh_host_ed25519_key and place the private key
  # it is used to decrypt the secrets with sops

  # install
  ./bare-install.sh

  ```
3. Reboot

## Flake Install

> WARNING: This isn't working yet as of 2023-04-24 because of
> ```error: filesystem error: cannot rename: Invalid cross-device link [/mnt/nix/store/1caynpxvnlc0a6bcyr35z4gpjiy1qacq-manifest.json.drv.chroot/nix/store/5b37nyxpghm0h2fsx8nn23qh9mpp3f65-manifest.json] [/nix/store/5b37nyxpghm0h2fsx8nn23qh9mpp3f65-manifest.json]```
> So in the meantime use the "Bare Install"


1. Boot into installer

2. Follow:
  ``` sh
  # go into a root shell
  sudo su

  # create this folder if necessary
  mkdir -p /mnt/etc/

  git clone https://github.com/ramblurr/nixcfg.git /mnt/etc/nixos --recurse-submodules

  cd /mnt/etc/nixos/hosts/stable/x86_64-linux/mali

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
