{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ovos-kitchen";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ../../../home.nix
    inputs.nixos-ovos.nixosModules.ovos
    inputs.nixos-raspberrypi.nixosModules.base
    inputs.nixos-raspberrypi.nixosModules.hardware
  ];
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "feedb33df4cc4cbf8e64e91cf837d8b2";

  raspberry-pi.hardware.hifiberry-dacplusadc.enable = true;
  raspberry-pi.hardware.platform.type = "rpi4";
  services.timesyncd.enable = true;

  ovos.password.enable = true;
  ovos.gui.enable = false;
  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    users.enable = true;
    users.primaryUser = {
      username = ramblurr.username;
      uid = 1001;
      name = ramblurr.name;
      homeDirectory = ramblurr.homeDirectory;
      signingKey = ramblurr.signingKey;
      email = ramblurr.email;
      passwordSecretKey = ramblurr.passwordSecretKey;
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
      ];
    };
  };
}
