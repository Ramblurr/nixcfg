{
  lib,
  inputs,
  modulesPath,
  ...
}:
{
  imports = [
    inputs.nixos-generators.nixosModules.all-formats
  ];
  # Disable base.nix profile imported by sd-image module.
  # It adds recovery tools like w3m, testdisk, ddrescue that aren't needed on deployed RPi.
  disabledModules = [ "${modulesPath}/profiles/base.nix" ];
  formatConfigs.sd-aarch64 =
    { lib, ... }:
    {
      # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low on space.
      sdImage.compressImage = false;
      # ref: https://github.com/nix-community/nixos-generators/issues/379
      fileExtension = lib.mkForce ".img*";
    };
  hardware.enableRedistributableFirmware = true;
  networking.networkmanager.enable = lib.mkForce false;
  services.openssh.settings = {
    PermitRootLogin = lib.mkForce "prohibit-password";
    PasswordAuthentication = false;
    KbdInteractiveAuthentication = false;
    StreamLocalBindUnlink = true;
  };
  # disable the zfs kernel module
  # takes too long to build and we don't need zfs on the raspberry pi anyways
  nixpkgs.overlays = [
    (final: super: {
      zfs = super.zfs.overrideAttrs (_: {
        meta.platforms = [ ];
      });
    })
  ];
  services.fwupd.enable = false;
  services.udisks2.enable = false;
  programs.command-not-found.enable = false;
  boot.enableContainers = false;

  # there is a manual bootstrap step after flashing the sd image
  # one has to login, install the proper ssh key
  # then restart the sops-nix service
  services.getty.autologinUser = "root";
  users.users.root.initialHashedPassword = lib.mkForce null;
  users.users.root.hashedPassword = lib.mkForce null;
  users.users.root.hashedPasswordFile = lib.mkForce null;
}
