{
  config,
  pkgs,
  ...
}:

{
  imports = [
    ./disk-config.nix
    ./hardware.nix
    ./networking.nix
    ./matrix.nix
    ./hirde.nix
    ./ingress.nix
    ./skills.nix
    ./llm.nix
    ../../config
  ];

  system.stateVersion = "26.05";
  networking.hostName = "thingstead";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  # ZFS pools record this hostid; changing it blocks the next import.
  networking.hostId = "350d17ed";
  sops.defaultSopsFile = ./secrets.sops.yaml;
  documentation.enable = false;

  modules = {
    users = {
      enable = true;
      headless.enable = true;
      primaryUser.extraGroups = [ "wheel" ];
    };
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    editors.vim.enable = true;
    impermanence.enable = true;
    boot.zfs = {
      enable = true;
      encrypted = true;
      rootPool = "rpool";
      scrubPools = [ "rpool" ];
      usePlymouth = false;
    };
    zfs.datasets.enable = true;
    nix.pruneAgedGcroots.enable = true;
    vpn.tailscale.enable = true;
    microvm-host = {
      enable = true;
      baseZfsDataset = "rpool/encrypted/safe/microvms";
    };
  };
  home.nix-lan-cache.enable = true;

  security.sudo.wheelNeedsPassword = false;

  environment.persistence."/persist".directories = [
    "/var/lib/nixos"
    "/var/lib/systemd/coredump"
  ];

  environment.systemPackages = [
    pkgs.bandwhich
    pkgs.fd
    pkgs.htop
    pkgs.isd
    pkgs.lshw
    pkgs.ncdu
    pkgs.python3
    pkgs.rclone
    pkgs.ripgrep
    pkgs.smartmontools
    pkgs.tcpdump
    pkgs.tmux
    pkgs.vifm
    pkgs.yq-go
  ];

  nix.settings.narinfo-cache-positive-ttl = 3600;

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/postgresql"."mountpoint" = "/var/lib/postgresql";
    "rpool/encrypted/safe/svc/wrights/wright-000"."mountpoint" = "/var/lib/wrights/wright-000";
  };
}
