{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "mali";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./nfs.nix
    ./samba.nix
    ./zrepl.nix
    ./monitoring.nix
    ./acme.nix
    ./minio.nix
    ./syncthing.nix
    ./borgbackup-server.nix
    ./ups.nix
  ];
  # TODO
  # mail notifications ups, zfs issues
  # users
  #   zigbee2mqtt
  #   roon
  #   homeassistant / hassos?
  #   phoniebox ?
  #   media ?
  #
  # rclone
  # firewall check
  # impermanence for all custom config
  # zrepl use unstable https://github.com/danderson/homelab/blob/30b7aa0907831d07959323084e0e536d03a78219/lib/zrepl.nix#L4
  system.stateVersion = "23.05";
  sops.defaultSopsFile = defaultSopsFile;
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "3b3e54988be146febcce587e0f65669b";

  documentation.nixos.enable = false;
  documentation.doc.enable = false;
  modules = {
    shell = {
      atuin.enable = true;
      direnv.enable = true;
      htop.enable = true;
      tmux.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
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
      scrubPools = ["rpool"];
      autoSnapshot.enable = false;
      # extraPools = ["tank" "tank2" "fast"];
    };
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    networking.default.enable = true;
    networking.default.hostName = hn;
    users.primaryUser = {
      username = ramblurr.username;
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

  users.groups = {
    k8s-nfs.gid = 2000;
    proxmox.gid = 1004;
  };
  users.users = {
    k8s-nfs = {
      group = "k8s-nfs";
      uid = 2000;
      isSystemUser = true;
    };
    proxmox = {
      group = "proxmox";
      uid = 1004;
      isSystemUser = true;
    };
  };
  services.smartd.enable = true;
  environment.systemPackages = with pkgs; [
    dosfstools
    smartmontools
    ncdu
    rclone
    sshfs
    lshw
  ];
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [
    ];
  };
}
