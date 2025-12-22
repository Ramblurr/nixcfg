{
  config,
  pkgs,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  imports = [
    ./hardware.nix
    ../../config
    ./networking.nix
    ./modules/firewall
    ./modules/podman.nix
    ./modules/chrony.nix
    ./modules/kea
    ./modules/powerdns.nix
    ./modules/dnsdist.nix
    ./modules/maddy.nix
    ./modules/udpbroadcastrelay.nix
    ./modules/mullvad-gateway.nix
    ./modules/ntopng.nix
    ./modules/crowdsec.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";
  boot.loader.systemd-boot.enable = true;

  environment.systemPackages = with pkgs; [
    dogdns
    tcpdump
    wireguard-tools
    inetutils
    iperf3
  ];

  services.tailscale.enable = true;

  ############################
  ## My Custom Base Modules ##
  ############################
  modules = {
    telemetry = {
      smartd.enable = true;
      prometheus-node-exporter.enable = true;
      prometheus-zfs-exporter.enable = true;
      prometheus-smartctl-exporter.enable = true;
    };
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
    users.enable = true;
    users.headless.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
      "kvm"
      "libvirtd"
    ];
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
    "d /persist/home/${username} 700 ${username} ${username}"
    "d /persist/home/${username}/.config 0775 ${username} ${username}  -"
    "d /persist/home/${username}/.local 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state/zsh 755 ${username} ${username}"
  ];
}
