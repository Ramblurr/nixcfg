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
    ./email.nix
    ./samba.nix
    ./zrepl.nix
    ./monitoring.nix
    ./acme.nix
    ./nginx.nix
    ./minio.nix
    ./syncthing.nix
    ./borgbackup-server.nix
    ./ups.nix
    ./avahi.nix
  ];
  # TODO
  # rclone
  # firewall check
  # impermanence for all custom config
  # atuin sycn services.atuin https://github.com/Mic92/dotfiles/blob/3ffb89b624ea21d3be3584f44d1335237e3daf9a/nixos/eve/modules/atuin.nix#L6
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
      extraPools = ["tank" "tank2" "fast"];
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

  sops.secrets."tank2Key" = {
    mode = "400";
    owner = "root";
    group = "root";
  };

  sops.secrets."fastKey" = {
    mode = "400";
    owner = "root";
    group = "root";
  };

  environment.etc."mali-keys/tank2.key" = {
    user = "root";
    source = config.sops.secrets.tank2Key.path;
  };

  environment.etc."mali-keys/fast.key" = {
    user = "root";
    source = config.sops.secrets.fastKey.path;
  };

  users.groups = {
    k8s-nfs.gid = 2000;
    proxmox.gid = 1004;
    zigbee2mqtt.gid = 1006;
    roon.gid = 1017;
    hassos.gid = 1018;
    photo-backup.gid = 3000;
  };
  users.users = {
    k8s-nfs = {
      group = "k8s-nfs";
      uid = 2000;
      isSystemUser = true;
    };
    zigbee2mqtt = {
      group = "zigbee2mqtt";
      uid = 1006;
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        ''command="${pkgs.rrsync}/bin/rrsync /mnt/tank2/backups/zigbee2mqtt/",restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIILApoRF9K7265hxTEI9Frq4VEqpfeili/LdVfnt1zz4''
      ];
    };
    proxmox = {
      group = "proxmox";
      uid = 1004;
      isSystemUser = true;
    };
    roon = {
      group = "roon";
      isSystemUser = true;
      uid = 1007;
    };
    hassos = {
      group = "hassos";
      isSystemUser = true;
      uid = 1008;
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
