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
  mediaUid = home-ops.users.media.uid;
  mediaGroup = home-ops.groups.media.name;
  mediaGid = home-ops.groups.media.gid;
  qbittorrentDomain = "qbittorrent.${cfg.baseDomain}";
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
  gluetunStateDir = "${stateDirActual}/gluetun";
  overseerrStateDir = "${stateDirActual}/overseerr";
  qbittorrentStateDir = "${stateDirActual}/qbittorrent";
  qbittorrentConfigDir = "${qbittorrentStateDir}/qBittorrent";
  qbittorrentConfigFile = "${qbittorrentConfigDir}/qBittorrent.conf";
  qbittorrentPort = toString cfg.ports.qbittorrent;
  qbittorrentPortForwardUpCommand = "/bin/sh -c 'wget -O- -nv --retry-connrefused --post-data \"json={\\\"listen_port\\\":{{PORT}},\\\"current_network_interface\\\":\\\"{{VPN_INTERFACE}}\\\",\\\"random_port\\\":false,\\\"upnp\\\":false}\" http://127.0.0.1:${qbittorrentPort}/api/v2/app/setPreferences'";
  qbittorrentPortForwardDownCommand = "/bin/sh -c 'wget -O- -nv --retry-connrefused --post-data \"json={\\\"listen_port\\\":0,\\\"current_network_interface\\\":\\\"lo\\\"}\" http://127.0.0.1:${qbittorrentPort}/api/v2/app/setPreferences'";
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
      qbittorrent = lib.mkOption { type = lib.types.port; };
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
      "d ${dlLocalPath} 0770 ${mediaUser} ${mediaGroup}"
      "A ${dlLocalPath} - - - - d:group:${mediaGroup}:rwx"
      "d ${gluetunStateDir} 0700 root root"
      "d ${overseerrStateDir} 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentStateDir} 0770 ${mediaUser} ${mediaGroup}"
    ];

    sops.secrets."home-dl/gluetun-protonvpn.env" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = "root";
      mode = "0400";
      restartUnits = [ "home-dl-gluetun.service" ];
    };

    systemd.services.home-dl-qbittorrent-config = {
      description = "Prepare qBittorrent config for Gluetun port forwarding";
      before = [ "home-dl-qbittorrent.service" ];
      requiredBy = [ "home-dl-qbittorrent.service" ];
      unitConfig.RequiresMountsFor = [ stateDirActual ];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
      };
      script = ''
        install -d -m0770 -o ${mediaUser} -g ${mediaGroup} ${qbittorrentConfigDir}
        touch ${qbittorrentConfigFile}
        chown ${mediaUser}:${mediaGroup} ${qbittorrentConfigFile}
        chmod 0660 ${qbittorrentConfigFile}

        if grep -q '^WebUI\\LocalHostAuth=' ${qbittorrentConfigFile}; then
          sed -i 's/^WebUI\\LocalHostAuth=.*/WebUI\\LocalHostAuth=false/' ${qbittorrentConfigFile}
        else
          printf '\nWebUI\\LocalHostAuth=false\n' >> ${qbittorrentConfigFile}
        fi
      '';
    };

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
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
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
    };
    systemd.services.radarr = {
      description = "Radarr";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
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
        SupplementaryGroups = [ mediaUser ];
        ExecStart = "${lib.getExe pkgs.sabnzbd} -d -f ${stateDirEffective}/sabnzbd/sabnzbd.ini";
        WorkingDirectory = "${stateDirEffective}/sabnzbd";
        ReadWritePaths = [
          mediaLocalPath
          dlLocalPath
        ];
      }
      // sharedServiceConfig;
    };
    systemd.services.prowlarr = {
      description = "Prowlarr";
      after = [ "network.target" ] ++ serviceDeps;
      bindsTo = serviceDeps;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        SupplementaryGroups = [ mediaUser ];
        StateDirectory = "home-dl/prowlarr";
        ExecStart = "${lib.getExe pkgs.prowlarr} -nobrowser -data=${stateDirEffective}/prowlarr";
        Restart = "on-failure";
      }
      // sharedServiceConfig;
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

    virtualisation.quadlet.enable = true;
    virtualisation.quadlet = {
      networks.home-dl-torrent = {
        autoStart = true;
        networkConfig = {
          NetworkName = "home-dl-torrent";
        };
      };
      containers = {
        home-dl-gluetun = {
          autoStart = true;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          unitConfig = {
            RequiresMountsFor = [ stateDirActual ];
          };
          containerConfig = {
            # renovate: docker-image
            Image = "ghcr.io/qdm12/gluetun:v3.41.1";
            ContainerName = "home-dl-gluetun";
            Network = "home-dl-torrent.network";
            AddCapability = [ "NET_ADMIN" ];
            AddDevice = [ "/dev/net/tun:/dev/net/tun" ];
            PublishPort = [ "127.0.0.1:${toString cfg.ports.qbittorrent}:${toString cfg.ports.qbittorrent}" ];
            EnvironmentFile = [ config.sops.secrets."home-dl/gluetun-protonvpn.env".path ];
            Environment = [
              "VPN_SERVICE_PROVIDER=protonvpn"
              "VPN_TYPE=wireguard"
              "PORT_FORWARD_ONLY=on"
              "VPN_PORT_FORWARDING=on"
              "VPN_PORT_FORWARDING_UP_COMMAND=${qbittorrentPortForwardUpCommand}"
              "VPN_PORT_FORWARDING_DOWN_COMMAND=${qbittorrentPortForwardDownCommand}"
              "FIREWALL_INPUT_PORTS=${toString cfg.ports.qbittorrent}"
              "TZ=Europe/Berlin"
            ];
            Volume = [ "${gluetunStateDir}:/gluetun:rw" ];
            HealthCmd = "/gluetun-entrypoint healthcheck";
            HealthInterval = "30s";
            HealthTimeout = "10s";
            HealthRetries = 3;
            HealthStartPeriod = "30s";
            HealthOnFailure = "kill";
            Notify = "healthy";
          };
        };

        home-dl-overseerr = {
          # Intentionally uses Podman's default networking, not the Gluetun/qBittorrent VPN network.
          autoStart = true;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          unitConfig = {
            RequiresMountsFor = [ stateDirActual ];
          };
          containerConfig = {
            # renovate: docker-image
            Image = "lscr.io/linuxserver/overseerr:1.35.0";
            ContainerName = "home-dl-overseerr";
            PublishPort = [ "127.0.0.1:${toString cfg.ports.overseerr}:5055" ];
            User = toString mediaUid;
            Group = toString mediaGid;
            Environment = [
              "PUID=${toString mediaUid}"
              "PGID=${toString mediaGid}"
              "TZ=Europe/Berlin"
              "UMASK=007"
            ];
            Volume = [ "${overseerrStateDir}:/config:rw" ];
          };
        };

        home-dl-qbittorrent = {
          autoStart = true;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          unitConfig = {
            RequiresMountsFor = [
              stateDirActual
              dlLocalPath
            ];
          };
          containerConfig = {
            # renovate: docker-image
            Image = "lscr.io/linuxserver/qbittorrent:libtorrentv1";
            ContainerName = "home-dl-qbittorrent";
            Network = "home-dl-gluetun.container";
            User = toString mediaUid;
            Group = toString mediaGid;
            Environment = [
              "PUID=${toString mediaUid}"
              "PGID=${toString mediaGid}"
              "TZ=Europe/Berlin"
              "UMASK=007"
              "WEBUI_PORT=${toString cfg.ports.qbittorrent}"
              "TORRENTING_PORT=6881"
            ];
            Volume = [
              "${qbittorrentStateDir}:/config:rw"
              "${dlLocalPath}:/downloads:rw"
            ];
          };
        };
      };
    };

    modules.services.ingress.virtualHosts =
      (lib.mapAttrs' (
        _name: ingress:
        lib.nameValuePair ingress.domain {
          acmeHost = cfg.ingress.domain;
          upstream = "http://${lib.my.cidrToIp cfg.subnet.nsAddr}:${toString ingress.port}";
          inherit (ingress) forwardAuth;
        }
      ) ingresses)
      // {
        "${ingresses.overseerr.domain}" = {
          acmeHost = cfg.ingress.domain;
          upstream = "http://127.0.0.1:${toString cfg.ports.overseerr}";
          forwardAuth = false;
        };
        "${qbittorrentDomain}" = {
          acmeHost = cfg.ingress.domain;
          upstream = "http://127.0.0.1:${toString cfg.ports.qbittorrent}";
          forwardAuth = false;
        };
      };
  };
}
