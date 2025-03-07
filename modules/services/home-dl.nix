{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.home-dl;
  home-ops = config.repo.secrets.home-ops;
  ingresses = {
    radarr = {
      domain = "radarr.${cfg.baseDomain}";
      port = 7878;
      forwardAuth = true;
    };
    sonarr = {
      domain = "sonarr.${cfg.baseDomain}";
      port = 8989;
      forwardAuth = true;
    };
    prowlarr = {
      domain = "prowlarr.${cfg.baseDomain}";
      port = 9696;
      forwardAuth = true;
    };
    sabnzbd = {
      domain = "sabnzbd.${cfg.baseDomain}";
      port = 8080;
      forwardAuth = true;
    };
    overseerr = {
      domain = "requests.${cfg.baseDomain}";
      port = cfg.ports.overseerr;
      forwardAuth = false;
    };
  };
  stateDirActual = "/var/lib/private/home-dl";
  stateDirEffective = "/var/lib/home-dl";
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  dlLocalPath = "/mnt/downloads";
  serviceDeps = [
    "${utils.escapeSystemdPath mediaLocalPath}.mount"
    "${utils.escapeSystemdPath dlLocalPath}.mount"
    "${utils.escapeSystemdPath stateDirActual}.mount"
  ];
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
  options.modules.services.home-dl = {
    enable = lib.mkEnableOption "home-dl";
    baseDomain = lib.mkOption {
      type = lib.types.str;
      example = "example.com";
      description = "The base domaint to use for all services";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      overseerr = lib.mkOption { type = lib.types.port; };
    };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
    subnet = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = {
      "${cfg.ingress.domain}" = {
        externalDomains = [ ingresses.overseerr.domain ];
      };
    };

    fileSystems."${mediaLocalPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.mediaNfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/home-dl"."mountpoint" = "${stateDirActual}";
      "rpool/encrypted/safe/svc/home-dl"."com.sun:auto-snapshot" = "false";
      "tank/encrypted/downloads"."mountpoint" = "${dlLocalPath}";
      "tank/encrypted/downloads"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "d ${dlLocalPath} 0770 ${home-ops.users.media.name} ${home-ops.groups.media.name}"
      "A ${dlLocalPath} - - - - d:group:${home-ops.groups.media.name}:rwx"
    ];

    modules.networking.systemd-netns-private = {
      enable = true;
      namespaces.home-dl = {
        hostAddr = cfg.subnet.hostAddr;
        nsAddr = cfg.subnet.nsAddr;
        hostIface = "home-dl-host";
        nsIface = "home-dl-ns";
        services = [
          "radarr.service"
          "sonarr.service"
          "sabnzbd.service"
          "prowlarr.service"
          "overseerr.service"
          "recyclarr.service"
        ];
      };
    };
    systemd.services.sonarr = {
      description = "Sonarr";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        StateDirectory = "home-dl/sonarr";
        SupplementaryGroups = [ "${home-ops.users.media.name}" ];
        ExecStart = "${pkgs.sonarr}/bin/NzbDrone -nobrowser -data='${stateDirEffective}/sonarr'";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      } // sharedServiceConfig;
    };
    systemd.services.radarr = {
      description = "Radarr";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        StateDirectory = "home-dl/radarr";
        SupplementaryGroups = [ "${home-ops.users.media.name}" ];
        ExecStart = "${pkgs.radarr}/bin/Radarr -nobrowser -data='${stateDirEffective}/radarr'";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      } // sharedServiceConfig;
    };
    systemd.services.sabnzbd = {
      description = "sabnzbd server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      serviceConfig = {
        Type = "forking";
        GuessMainPID = "no";
        StateDirectory = "home-dl/sabnzbd";
        SupplementaryGroups = [ "${home-ops.users.media.name}" ];
        ExecStart = "${lib.getExe pkgs.sabnzbd} -d -f ${stateDirEffective}/sabnzbd/sabnzbd.ini";
        WorkingDirectory = "${stateDirEffective}/sabnzbd";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      } // sharedServiceConfig;
    };
    systemd.services.prowlarr = {
      description = "Prowlarr";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        SupplementaryGroups = [ "${home-ops.users.media.name}" ];
        StateDirectory = "home-dl/prowlarr";
        ExecStart = "${lib.getExe pkgs.prowlarr} -nobrowser -data=${stateDirEffective}/prowlarr";
        Restart = "on-failure";
      } // sharedServiceConfig;
    };
    systemd.services.overseerr = {
      enable = false;
      description = "Request management and media discovery tool for the Plex ecosystem";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      wantedBy = [ "multi-user.target" ];
      environment = {
        LOG_LEVEL = "info";
        PORT = toString cfg.ports.overseerr;
      };
      serviceConfig =
        {
          Type = "exec";
          StateDirectory = "home-dl/overseerr";
          WorkingDirectory = "${pkgs.overseerr}/libexec/overseerr/deps/overseerr";
          ExecStart = "${pkgs.overseerr}/bin/overseerr";
          BindPaths = [
            "/var/lib/home-dl/overseerr/:${pkgs.overseerr}/libexec/overseerr/deps/overseerr/config/"
          ];
        }
        // sharedServiceConfig
        // {
          PrivateMounts = true;
        };
    };

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
      } // sharedServiceConfig;
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

    modules.services.ingress.virtualHosts = lib.mapAttrs' (
      name: ingress:
      lib.nameValuePair ingress.domain {
        acmeHost = cfg.ingress.domain;
        upstream = "http://${lib.my.cidrToIp cfg.subnet.nsAddr}:${toString ingress.port}";
        forwardAuth = ingress.forwardAuth;
      }
    ) ingresses;
  };
}
