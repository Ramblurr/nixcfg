{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "mali";
in {
  imports = [
    ./hardware-configuration.nix
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
  system.stateVersion = "23.05";
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "3b3e54988be146febcce587e0f65669b";
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
    boot.zfs.enable = true;
    boot.zfs.encrypted = false;
    boot.zfs.rootPool = "rpool";
    boot.zfs.scrubPools = ["rpool"];
    boot.zfs.extraPools = ["tank" "tank2" "fast"];
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    networking.default.enable = true;
    networking.default.hostName = hn;
    users.primaryUser = {
      username = "ramblurr";
      name = "Casey Link";
      homeDirectory = "/home/ramblurr";
      signingKey = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
      email = "unnamedrambler@gmail.com";
      passwordSecretKey = "ramblurr-password";
      shell = pkgs.zsh;
      defaultSopsFile = ./secrets-mali.sops.yaml;
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
    };
    proxmox = {
      group = "proxmox";
      uid = 1004;
    };
  };
  services.smartd.enable = true;
  environment.systemPackages = with pkgs; [
    smartmontools
    ncdu
    rclone
    sshfs
  ];
  # TODO
  # users
  # zigbee2mqtt
  # roon
  # homeassistant / hassos?
  # phoniebox ?
  # media ?
}
