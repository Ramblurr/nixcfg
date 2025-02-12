{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hn = "addams";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../ramblurr.nix {
    inherit
      config
      lib
      pkgs
      inputs
      ;
  };
in
{
  imports = [
    ./hardware.nix
    ../../config/secrets.nix
    ./networking.nix
    ./modules/firewall
    ./modules/podman.nix
    ./modules/chrony.nix
    ./modules/kea
    ./modules/powerdns.nix
    ./modules/dnsdist.nix
    #./modules/maddy.nix
    ./modules/udpbroadcastrelay.nix
    ./modules/mullvad-gateway.nix
    ./modules/ntopng.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  boot.loader.systemd-boot.enable = true;

  environment.systemPackages = with pkgs; [
    tcpdump
    wireguard-tools
    inetutils
    iperf3
    pdns
  ];

  ############################
  ## My Custom Base Modules ##
  ############################
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
    };
    zfs.datasets.enable = true;
    security.default.enable = true;
    # since this is my router, we handle networking and firealling in this host config
    firewall.enable = false;
    networking.default.enable = false;
    users.enable = true;

    users.primaryUser = ramblurr // {
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
        "kvm"
        "libvirtd"
      ];
    };
  };

  ######################
  # Impermanence Setup #
  ######################
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [ ];
  };

  systemd.tmpfiles.rules = [
    "d /persist/home/${ramblurr.username} 700 ${ramblurr.username} ${ramblurr.username}"
    "d /persist/home/${ramblurr.username}/.config 0775 ${ramblurr.username} ${ramblurr.username}  -"
    "d /persist/home/${ramblurr.username}/.local 755 ${ramblurr.username} ${ramblurr.username}"
    "d /persist/home/${ramblurr.username}/.local/state 755 ${ramblurr.username} ${ramblurr.username}"
    "d /persist/home/${ramblurr.username}/.local/state/zsh 755 ${ramblurr.username} ${ramblurr.username}"
  ];

  services.smartd.enable = true;
}
