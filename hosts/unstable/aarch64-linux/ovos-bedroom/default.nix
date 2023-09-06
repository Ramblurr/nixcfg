{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ovos-bedroom";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ../../../home.nix
    ../pipewire.nix
    ../hivemindsat.nix
    inputs.nixos-raspberrypi.nixosModules.base
    inputs.nixos-raspberrypi.nixosModules.hardware
    inputs.nixos-raspberrypi.inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  system.stateVersion = "23.11";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "ccf2e2f97ab74fa49a7968b5d772565b";

  raspberry-pi.hardware.platform.type = "rpi3";
  raspberry-pi.hardware.apply-overlays-dtmerge.enable = true;
  environment.systemPackages = with pkgs; [
    roc-toolkit
    openfec
    alsaUtils
    i2c-tools
  ];

  networking.hostName = hn;
  services.nscd.enableNsncd = false;

  modules = {
    hardware = {
      udisks2.enable = false;
      fwupd.enable = false;
    };
    firewall.enable = true;
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
        "i2c"
      ];
    };
  };
  # ALSA only
  #sound.enable = true;
  #hardware.pulseaudio.enable = false;
  #services.pipewire.enable = false;
  #systemd.services = {
  #roc-send = {
  #  path = with pkgs; [
  #    roc-toolkit
  #  ];
  #  enable = true;
  #  description = "roc-send remote audio to a ovos instance";
  #  script = "${pkgs.roc-toolkit}/bin/roc-send --input alsa://hw:1,0 --source ovos-kitchen.int.***REMOVED***:10001 --repair ovos-kitchen.int.***REMOVED***:10002";
  #  wants = ["network-online.target"];
  #  after = ["network-online.target"];
  #  wantedBy = ["default.target"];
  #  serviceConfig = {
  #    Type = "simple";
  #    User = ramblurr.username;
  #    Group = ramblurr.username;
  #  };
  #};
  #};
}
