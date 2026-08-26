{
  config,
  lib,
  utils,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.home-dl;
  inherit (config.repo.secrets) home-ops;
  mediaUser = home-ops.users.media.name;
  mediaGroup = home-ops.groups.media.name;
  ingresses = {
    radarr = {
      domain = "radarr.${cfg.baseDomain}";
      port = 7878;
      healthCheckPath = "/ping";
      forwardAuth = true;
    };
    sonarr = {
      domain = "sonarr.${cfg.baseDomain}";
      port = 8989;
      healthCheckPath = "/ping";
      forwardAuth = true;
    };
    prowlarr = {
      domain = "prowlarr.${cfg.baseDomain}";
      port = 9696;
      healthCheckPath = "/ping";
      forwardAuth = true;
    };
    sabnzbd = {
      domain = "sabnzbd.${cfg.baseDomain}";
      port = 8080;
      healthCheckPath = "/api?mode=version&output=json";
      forwardAuth = true;
    };
  };
  stateDirActual = "/var/lib/private/home-dl";
  stateDirEffective = "/var/lib/home-dl";
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  dlLocalPath = "/mnt/downloads";
  nfsMountDeps = [ "${utils.escapeSystemdPath mediaLocalPath}.mount" ];
  stateDataset = "rpool/encrypted/safe/svc/home-dl";
  downloadsDataset = "tank/encrypted/downloads";
  downloadServices = [
    "prowlarr"
    "qbittorrent"
    "radarr"
    "sabnzbd"
    "sonarr"
  ];
  stateServices = downloadServices ++ [ "qui" "recyclarr" ];
  sharedSystemdService = {
    after = [ "network.target" ] ++ nfsMountDeps;
    bindsTo = nfsMountDeps;
  };
  sharedServiceConfig = {
    UMask = 77;
    DynamicUser = true;
    RestartSec = "10s";
    Restart = "on-failure";
    ProtectHome = true;
    ProtectSystem = "strict";
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectHostname = true;
    ProtectClock = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectKernelLogs = true;
    ProtectControlGroups = true;
    NoNewPrivileges = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
    RemoveIPC = true;
    LockPersonality = true;
    PrivateMounts = false; # Cannot be used because we are using a network namespace (ref: https://github.com/systemd/systemd/issues/2741#issuecomment-1447387577)
    PrivateUsers = true;
    RestrictNamespaces = true;
    CapabilityBoundingSet = "";
    SystemCallArchitectures = "native";
    #SystemCallFilter =[ "@system-service" ]; # breaks radarr, need to circle back to this with strace or shh
    #MemoryDenyWriteExecute = true; # does not work on Mono apps like sonarr
  };
  recyclarrYaml = pkgs.writeTextFile {
    name = "recyclarr.yml";
    text = builtins.readFile ../../configs/home-ops/recyclarr-plato.yml;
  };
in
{
  imports = [ ./home-dl/qbittorrent.nix ];
  options.modules.services.home-dl = {
    enable = lib.mkEnableOption "home-dl";
    baseDomain = lib.mkOption {
      type = lib.types.str;
      example = "example.com";
      description = "The base domaint to use for all services";
    };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
    subnet = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {

    fileSystems."${mediaLocalPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.mediaNfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets = {
      properties = {
        ${stateDataset} = {
          mountpoint = stateDirActual;
          "com.sun:auto-snapshot" = "false";
        };
        ${downloadsDataset} = {
          mountpoint = dlLocalPath;
          "com.sun:auto-snapshot" = "false";
        };
      };
      services = {
        ${stateDataset} = stateServices;
        ${downloadsDataset} = downloadServices;
      };
    };

    systemd.tmpfiles.rules = [
      "d ${dlLocalPath} 0770 ${mediaUser} ${mediaGroup}"
      "A ${dlLocalPath} - - - - d:group:${mediaGroup}:rwx"
    ];

    modules.networking.systemd-netns-private = {
      enable = true;
      namespaces.home-dl = {
        inherit (cfg.subnet) hostAddr;
        inherit (cfg.subnet) nsAddr;
        hostIface = "home-dl-host";
        nsIface = "home-dl-ns";
        services = [
          "radarr.service"
          "sonarr.service"
          "sabnzbd.service"
          "prowlarr.service"
          "recyclarr.service"
        ];
      };
    };
    systemd.services.sonarr = {
      description = "Sonarr";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        StateDirectory = "home-dl/sonarr";
        SupplementaryGroups = [ mediaUser ];
        ExecStart = "${pkgs.sonarr}/bin/NzbDrone -nobrowser -data='${stateDirEffective}/sonarr'";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      }
      // sharedServiceConfig;
    }
    // sharedSystemdService;
    systemd.services.radarr = {
      description = "Radarr";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        StateDirectory = "home-dl/radarr";
        SupplementaryGroups = [ mediaUser ];
        ExecStart = "${pkgs.radarr}/bin/Radarr -nobrowser -data='${stateDirEffective}/radarr'";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      }
      // sharedServiceConfig;
    }
    // sharedSystemdService;
    systemd.services.sabnzbd = {
      description = "sabnzbd server";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "forking";
        GuessMainPID = "no";
        StateDirectory = "home-dl/sabnzbd";
        SupplementaryGroups = [ mediaUser ];
        ExecStart = "${lib.getExe pkgs.sabnzbd} -d -f ${stateDirEffective}/sabnzbd/sabnzbd.ini";
        WorkingDirectory = "${stateDirEffective}/sabnzbd";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      }
      // sharedServiceConfig;
    }
    // sharedSystemdService;
    systemd.services.prowlarr = {
      description = "Prowlarr";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        SupplementaryGroups = [ mediaUser ];
        StateDirectory = "home-dl/prowlarr";
        ExecStart = "${lib.getExe pkgs.prowlarr} -nobrowser -data=${stateDirEffective}/prowlarr";
        Restart = "on-failure";
      }
      // sharedServiceConfig;
    }
    // sharedSystemdService;
    #sops.secrets."home-dl/sonarr/apiKey" = { };
    #sops.secrets."home-dl/radarr/apiKey" = { };
    systemd.services.recyclarr = {
      description = "Recyclarr Sync Service";
      preStart = ''
        install -m600 ${recyclarrYaml} ${stateDirEffective}/recyclarr/recyclarr.yaml
        # we are using PrivateTmp=true
        ${lib.getExe pkgs.xmlstarlet} sel -t -v "//ApiKey" -nl $CREDENTIALS_DIRECTORY/sonarr.xml > /tmp/sonarr.api_key
        ${lib.getExe pkgs.xmlstarlet} sel -t -v "//ApiKey" -nl $CREDENTIALS_DIRECTORY/radarr.xml > /tmp/radarr.api_key
        ${pkgs.replace-secret}/bin/replace-secret 'SONARR_API_KEY' /tmp/sonarr.api_key ${stateDirEffective}/recyclarr/recyclarr.yaml
        ${pkgs.replace-secret}/bin/replace-secret 'RADARR_API_KEY' /tmp/radarr.api_key ${stateDirEffective}/recyclarr/recyclarr.yaml
      '';
      environment = {
        XDG_CONFIG_HOME = "${stateDirEffective}/recyclarr";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.recyclarr}/bin/recyclarr sync --config ${stateDirEffective}/recyclarr/recyclarr.yaml";
        LoadCredential = [
          "sonarr.xml:${stateDirEffective}/sonarr/config.xml"
          "radarr.xml:${stateDirEffective}/radarr/config.xml"
        ];
        StateDirectory = "home-dl/recyclarr";
        ReadOnlyPaths = [ recyclarrYaml ];
      }
      // sharedServiceConfig;
    };

    systemd.timers.recyclarr = {
      description = "Recyclarr Sync Timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = [
          ""
          "daily"
        ];
        Persistent = true;
      };
    };

    site.gatus.heartbeats.recyclarr = {
      service = "recyclarr";
      name = "Recyclarr Sync";
      group = config.site.gatus.groups.media;
      interval = "30h";
    };
    site.gatus.endpoints = [
      {
        name = "Prowlarr";
        group = config.site.gatus.groups.media;
        url = "https://${ingresses.prowlarr.domain}/_health/gatus";
      }
      {
        name = "Radarr";
        group = config.site.gatus.groups.media;
        url = "https://${ingresses.radarr.domain}/_health/gatus";
      }
      {
        name = "SABnzbd";
        group = config.site.gatus.groups.media;
        url = "https://${ingresses.sabnzbd.domain}/_health/gatus";
      }
      {
        name = "Sonarr";
        group = config.site.gatus.groups.media;
        url = "https://${ingresses.sonarr.domain}/_health/gatus";
      }
    ];

    modules.services.caddy.protectedRoutes = lib.mapAttrs (_name: ingress: {
      inherit (ingress) healthCheckPath;
      publicHost = ingress.domain;
      upstream = "http://${lib.my.cidrToIp cfg.subnet.nsAddr}:${toString ingress.port}";
    }) ingresses;
  };
}
