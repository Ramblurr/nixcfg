{
  config,
  pkgs,
  lib,
  unstable,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
  defaultSopsFile = ./secrets.sops.yaml;
  inherit (config.repo.secrets) home-ops;
in
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./nfs.nix
    ./samba.nix
    ./zrepl.nix
    ./acme.nix
    ./nginx.nix
    ./minio.nix
    ./syncthing.nix
    ./borgbackup-server.nix
    ./ups.nix
    ./avahi.nix
    ./beets.nix
    ./atticd.nix
    ./ncps.nix
    ./rclone.nix
    ../../config
    ../../modules/site-net
  ];
  system.stateVersion = "23.05";
  sops.defaultSopsFile = defaultSopsFile;
  time.timeZone = "Europe/Berlin";
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;

  documentation.nixos.enable = false;
  documentation.doc.enable = false;
  modules = {
    telemetry = {
      smartd.enable = true;
      prometheus-node-exporter.enable = true;
      prometheus-zfs-exporter.enable = true;
      prometheus-smartctl-exporter.enable = true;
      prometheus-nut-exporter.enable = true;
      prometheus-ipmi-exporter.enable = true;
    };
    shell = {
      atuin.enable = true;
      attic.enable = true;
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
      zed.enable = true;
      encrypted = true;
      rootPool = "rpool2";
      scrubPools = [ "rpool2" ];
      autoSnapshot.enable = false;
      extraPools = [
        "tank"
        "tank2"
        "fast"
      ];
    };
    server.smtp-external-relay.enable = true;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
      "k8s-nfs"
    ];
  };

  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
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

  users.groups = (removeAttrs home-ops.groups [ "media" ]) // {
    k8s-nfs.gid = 2000;
    proxmox.gid = 1004;
    zigbee2mqtt.gid = 1006;
    roon.gid = 1017;
    hassos.gid = 1018;
    photo-backup.gid = 3000;
    atticd.gid = 1019;
  };
  users.users = (removeAttrs home-ops.users [ "media" ]) // {
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
    atticd = {
      group = "atticd";
      isSystemUser = true;
      uid = 1009;
    };
    plex = home-ops.users.plex // {
      extraGroups = [ "k8s-nfs" ];
    };
  };
  environment.systemPackages = with pkgs; [

    ipmitool
    lm_sensors
    unstable.rsgain
    mktorrent
    tcpdump
    python3
    dosfstools
    smartmontools
    ncdu
    rclone
    sshfs
    lshw
    vifm
    jless
  ];
  systemd.services.fix-media-perms = {
    description = "Fix media file permissions";
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      ExecStart = "/mnt/tank2/media/fix-media.sh";
    };
  };

  systemd.timers.fix-media-perms = {
    description = "Run fix-media-perms service hourly";
    wantedBy = [ "timers.target" ];
    enable = true;
    timerConfig = {
      OnUnitActiveSec = "1h";
      OnBootSec = "5min";
      Persistent = true;
    };
  };
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [ ];

    users.${username} = {
      directories = [
        ".config/rclone"
        ".config/qobuz-dl"
      ];
    };
  };
}
