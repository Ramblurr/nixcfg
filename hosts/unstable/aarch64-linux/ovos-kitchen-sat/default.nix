{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ovos-kitchen-sat";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ../../../home.nix
    inputs.nixos-raspberrypi.nixosModules.base
    inputs.nixos-raspberrypi.nixosModules.hardware
  ];
  system.stateVersion = "23.11";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "ccf2e2f97ab74fa49a7968b5d772565b";

  raspberry-pi.hardware.platform.type = "rpizero2";
  home.wifi.iot.enable = true;
  hardware.enableRedistributableFirmware = true;
  services.timesyncd.enable = true;
  #modules = {
  #  shell = {
  #    htop.enable = true;
  #    tmux.enable = true;
  #    zsh.enable = true;
  #    zsh.starship.enable = false;
  #  };
  #  services = {
  #    sshd.enable = true;
  #  };
  #  editors = {
  #    vim.enable = true;
  #  };
  #  users.enable = true;
  #  users.primaryUser = {
  #    username = ramblurr.username;
  #    name = ramblurr.name;
  #    homeDirectory = ramblurr.homeDirectory;
  #    signingKey = ramblurr.signingKey;
  #    email = ramblurr.email;
  #    passwordSecretKey = ramblurr.passwordSecretKey;
  #    defaultSopsFile = defaultSopsFile;
  #    shell = pkgs.zsh;
  #    extraGroups = [
  #      "wheel"
  #    ];
  #  };
  #};
}
