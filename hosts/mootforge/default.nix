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
    ./moot.nix
    ./nixbot.nix
    ../../config
    ../../modules/site-net
  ];

  system.stateVersion = "26.11";
  # Nixbot's flake prefetch and the tarmac plugin need upstream Nix.
  determinate.enable = false;
  networking.hostName = "mootforge";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  documentation.enable = false;

  modules = {
    motd = {
      enable = true;
      disks = [
        "/"
        "/nix"
        "/persist"
        "/boot"
      ];
      units = [
        config.systemd.services.nixbot.name
        config.systemd.services.postgresql.name
      ];
    };
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
    nix.buildLimits.enable = true;
    microvm-host = {
      enable = true;
      baseZfsDataset = "rpool/encrypted/safe/microvms";
    };
    services.onepassword-systemd-credentials.enable = true;
  };
  home.nix-lan-cache.enable = true;

  security.sudo.wheelNeedsPassword = false;

  environment.persistence."/persist".directories = [
    "/var/lib/nixos"
    "/var/lib/systemd/coredump"
  ];

  environment.systemPackages = [
    pkgs.bandwhich
    pkgs.ethtool
    pkgs.fd
    pkgs.htop
    pkgs.isd
    pkgs.lshw
    pkgs.ncdu
    pkgs.ripgrep
    pkgs.smartmontools
    pkgs.tcpdump
    pkgs.tmux
  ];
}
