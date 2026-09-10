{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ../../config
    ../../config/home-ops.nix
  ];
  system.stateVersion = "25.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";

  # Bootstrap on the existing untagged LAN. Enable the home-ops profile after
  # assigning management/data networks; it assumes static addresses and a tank pool.
  home-ops.enable = false;
  networking.hostId = lib.my.generateHostId config.networking.hostName;
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.networks."10-lan" = {
    matchConfig.PermanentMACAddress = config.repo.secrets.local.lan0.hwaddr;
    networkConfig.DHCP = "ipv4";
    linkConfig.RequiredForOnline = "routable";
  };
  services.resolved.enable = true;
  systemd.sleep.settings.Sleep = {
    AllowSuspend = false;
    AllowHibernation = false;
    AllowHybridSleep = false;
    AllowSuspendThenHibernate = false;
  };
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.systemPackages = [
    pkgs.smartmontools
    pkgs.gptfdisk
  ];
  documentation.nixos.enable = false;

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

    boot.zfs = {
      enable = true;
      encrypted = true;
      rootPool = "rpool";
      scrubPools = [ "rpool" ];
      extraPools = [ ];
      autoSnapshot.enable = false;
      usePlymouth = false;
    };
    zfs.datasets.enable = true;
    security.default.enable = true;
    firewall.enable = true;
    users.enable = true;
    users.headless.enable = true;
    telemetry.smartd.enable = true;
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
