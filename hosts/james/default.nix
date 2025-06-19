{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (config.modules.users.primaryUser) username;
in
{
  imports = [
    ./disk-config.nix
    ./hardware.nix
    ../../config/offsite.nix
    ../../config/hetzner-cloud-ccx.nix
    ./ingress.nix
    ./web.nix
    ./web/work.nix
    ./web/personal.nix
    ./web/personals.nix
    ./web/personal3.nix
  ];
  system.stateVersion = "24.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";

  networking.hostId = lib.my.generateHostId hostName;
  networking.firewall.enable = false;
  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    impermanence.enable = true;
    vpn.tailscale.enable = true;
    boot.zfs = {
      enable = false;
      encrypted = false;
      rootPool = "rpool";
      scrubPools = [ "rpool" ];
      extraPools = [ ];
      autoSnapshot.enable = false;
    };
    zfs.datasets.enable = true;
    security.default.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
    ];
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [ ];
  };

  systemd.tmpfiles.rules = [
    "d /persist/home/${username} 700 ${username} ${username}"
    "d /persist/home/${username}/.config 0775 ${username} ${username}  -"
    "d /persist/home/${username}/.local 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state/zsh 755 ${username} ${username}"
  ];
}
