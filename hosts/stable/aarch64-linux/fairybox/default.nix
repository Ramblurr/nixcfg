{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "fairybox";
  machine-id = "1b2d9977b3bb44508d67a72c7425828b";
  defaultSopsFile = ./secrets.sops.yaml;

  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    inputs.nixos-raspberrypi-stable.nixosModules.base
    inputs.nixos-raspberrypi-stable.nixosModules.hardware
    inputs.nixos-raspberrypi-stable.inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  system.stateVersion = "23.11";

  environment.etc."machine-id".text = machine-id;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  sops.defaultSopsFile = defaultSopsFile;

  raspberry-pi.hardware.hifiberry-dac.enable = true;
  raspberry-pi.hardware.platform.type = "rpi4";
  modules = {
    networking.default.enable = true;
    networking.default.hostName = hn;
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    hardware = {
      udisks2.enable = false;
      fwupd.enable = false;
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
