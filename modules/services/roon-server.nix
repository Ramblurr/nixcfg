{
  config,
  fetchurl,
  pkgs,
  lib,
  utils,
  unstable,
  ...
}:
let
  cfg = config.modules.services.roon-server;
  name = "roon-server";
  user = "roon-server";
  group = "roon-server";
  bluOSHosts = [
    "10.9.4.15"
    "10.9.4.12"
    "10.9.6.16"
    "10.9.4.13"
  ];

  toRebootCmdLine = map (
    host: "${lib.getExe pkgs.curl} -X POST http://${host}/reboot --data-raw 'noheader=0&yes='"
  ) bluOSHosts;

  rebootBluOS = pkgs.writeScript "reboot-bluos.sh" ''
    #!${pkgs.runtimeShell} -e
    echo "Rebooting bluos devices"
    ${lib.concatStringsSep "\n" toRebootCmdLine}
  '';
  stateDirActual = "/var/lib/private/roon-server";
  stateDirEffective = "/var/lib/roon-server";
  serviceDeps = map (path: "${utils.escapeSystemdPath path}.mount") [
    stateDirActual
    "/mnt/roon/backup"
    "/mnt/roon/music-other"
    "/mnt/roon/music-mine"
    "/mnt/roon/audiobooks"
  ];
in
{
  options.modules.services.roon-server = {
    enable = lib.mkEnableOption "roon-server";
  };
  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPorts = [
        33399 # Roon ARC
      ];
      allowedUDPPorts = [
        33399 # Roon ARC
        9003
      ];
      allowedTCPPortRanges = [
        {
          from = 9100;
          to = 9200;
        }
        {
          from = 9330;
          to = 9339;
        }
        {
          from = 30000;
          to = 30010;
        }
      ];
      extraInputRules = ''
        ip saddr { 224.0.0.0/4, 240.0.0.0/5 } accept
        ip daddr 224.0.0.0/4 accept
        pkttype { multicast, broadcast } accept
      '';
    };
    systemd.services.roon-server = {
      enable = true;
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      description = "Roon Server";
      wantedBy = [ "multi-user.target" ];
      environment = {
        ROON_DATAROOT = stateDirEffective;
        ROON_ID_DIR = stateDirActual;
      };
      serviceConfig = {
        ExecStart = "${lib.getExe pkgs.roon-server}";
        #ExecStart = lib.mkForce "${unstable.roon-server}/bin/RoonServer";
        ExecStartPost = rebootBluOS;
        StateDirectory = "roon-server";
        SupplementaryGroups = [
          "audio"
          "media"
        ];
        RestartSec = "30";
        Restart = "always";
        LimitNOFILE = 8192;
        IOWeight = "200"; # default, when unspecified is 100
        OOMScoreAdjust = "-500"; # default, when unspecified is 0. lower means less likely to be oom-killed
        Nice = "-2"; # default, when unspecified is 0
        CPUWeight = "300"; # default, when unspecified is 100
        MemoryLow = "2G";
        MemoryHigh = "8G";
        DynamicUser = true;
        ReadWritePaths = [ "/mnt/roon/backup" ];
        ReadOnlyPaths = [
          "/mnt/roon/music-other"
          "/mnt/roon/music-mine"
          "/mnt/roon/audiobooks"
        ];
      };
    };

    fileSystems."/mnt/roon/backup" = {
      device = "10.9.10.10:/mnt/tank2/backups/roon";
      fsType = "nfs";
    };
    fileSystems."/mnt/roon/music-other" = {
      device = "10.9.10.10:/mnt/tank2/media/music/other";
      fsType = "nfs";
    };
    fileSystems."/mnt/roon/music-mine" = {
      device = "10.9.10.10:/mnt/tank2/media/music/mine";
      fsType = "nfs";
    };
    fileSystems."/mnt/roon/audiobooks" = {
      device = "10.9.10.10:/mnt/tank2/media/audiobooks";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/roon-server"."mountpoint" = "/var/lib/private/roon-server";
      "rpool/encrypted/safe/svc/roon-server"."com.sun:auto-snapshot" = "false";
    };
  };
}
