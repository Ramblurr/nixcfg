{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.modules.services.opencloud;
  instances = lib.filterAttrs (_: i: i.enable) cfg.instances;
  values = builtins.attrValues instances;
  unique = xs: builtins.length xs == builtins.length (lib.unique xs);
  yaml = pkgs.formats.yaml { };
  instanceModule = { name, ... }: {
    options = {
      enable = lib.mkEnableOption "this OpenCloud instance" // {
        default = true;
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "opencloud-${name}";
      };
      uid = lib.mkOption {
        type = lib.types.ints.positive;
        description = "Stable host/guest service UID, also owning the NFS export.";
      };
      gid = lib.mkOption {
        type = lib.types.ints.positive;
        description = "Stable service GID, also owning the NFS export.";
      };
      stateDir = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/opencloud-${name}";
        description = "Local durable storage and rootless Podman home; never NFS.";
      };
      cacheDir = lib.mkOption {
        type = lib.types.str;
        default = "/var/cache/opencloud-${name}";
      };
      dataMount = lib.mkOption {
        type = lib.types.str;
        description = "An existing NFS mount owned by this service UID/GID. The module creates users and metadata below it only after verifying the mount.";
      };
      dataSource = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional expected NFS source, as reported by findmnt (server:/export).";
      };
      domain = lib.mkOption { type = lib.types.str; };
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Published address. Use the private guest address for a remote Caddy; firewall it separately.";
      };
      ports.app = lib.mkOption { type = lib.types.port; };
      ports.office = lib.mkOption { type = lib.types.port; };
      image = lib.mkOption {
        type = lib.types.str;
        default = "docker.io/opencloudeu/opencloud-rolling:7.5.0@sha256:6db1cfb06d430a663f16e9f33dcd4596d82a4875be0b4df233c26ce5f667ea74";
      };
      environmentFile = lib.mkOption {
        type = lib.types.str;
        description = "Runtime environment file readable only by this service account. Supply IDM_ADMIN_PASSWORD for initialization, SMTP secrets and other private settings here, not in Nix strings.";
      };
      environment = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "Non-secret upstream OpenCloud settings, including SMTP configuration.";
      };
      oidc = {
        issuer = lib.mkOption { type = lib.types.str; };
        clientId = lib.mkOption {
          type = lib.types.str;
          default = "web";
        };
      };
      office = {
        domain = lib.mkOption { type = lib.types.str; };
        image = lib.mkOption {
          type = lib.types.str;
          default = "ghcr.io/euro-office/documentserver@sha256:889e681923d2dcc8bdfb92fe128d10e185fcff880d302b6a0c0c7bf339499290";
          description = "Pinned Euro Office image (digest or local docker-archive).";
        };
        environmentFile = lib.mkOption {
          type = lib.types.str;
          description = "Private runtime file containing JWT_SECRET.";
        };
        environment = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
        };
      };
    };
  };
  mkInstance =
    name: i:
    let
      unit = "opencloud-${name}";
      officeUnit = "${unit}-office";
      envList = attrs: lib.mapAttrsToList (key: value: "${key}=${value}") attrs;
      officeProbeAddress = if i.listenAddress == "0.0.0.0" then "127.0.0.1" else i.listenAddress;
      csp = yaml.generate "${unit}-csp.yaml" {
        directives = {
          default-src = [ "'none'" ];
          base-uri = [ "'self'" ];
          child-src = [ "'self'" ];
          connect-src = [
            "'self'"
            "blob:"
            i.oidc.issuer
          ];
          font-src = [ "'self'" ];
          frame-ancestors = [ "'self'" ];
          frame-src = [
            "'self'"
            "blob:"
            "https://${i.office.domain}"
            i.oidc.issuer
          ];
          img-src = [
            "'self'"
            "data:"
            "blob:"
            "https://${i.office.domain}"
          ];
          manifest-src = [ "'self'" ];
          media-src = [ "'self'" ];
          object-src = [
            "'self'"
            "blob:"
          ];
          script-src = [
            "'self'"
            "'unsafe-inline'"
          ];
          style-src = [
            "'self'"
            "'unsafe-inline'"
            "blob:"
          ];
          worker-src = [
            "'self'"
            "blob:"
          ];
        };
      };
      registry = yaml.generate "${unit}-app-registry.yaml" {
        app_registry.mimetypes =
          map
            (entry: {
              mime_type = entry.mime;
              extension = entry.ext;
              name = entry.ext;
              description = entry.ext;
              default_app = "Euro-Office";
              allow_creation = true;
            })
            [
              {
                ext = "docx";
                mime = "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
              }
              {
                ext = "xlsx";
                mime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
              }
              {
                ext = "pptx";
                mime = "application/vnd.openxmlformats-officedocument.presentationml.presentation";
              }
              {
                ext = "odt";
                mime = "application/vnd.oasis.opendocument.text";
              }
              {
                ext = "ods";
                mime = "application/vnd.oasis.opendocument.spreadsheet";
              }
              {
                ext = "odp";
                mime = "application/vnd.oasis.opendocument.presentation";
              }
            ];
      };
      prepare = pkgs.writeShellScript "${unit}-prepare" ''
        set -eu
        ${pkgs.util-linux}/bin/findmnt --noheadings --mountpoint ${lib.escapeShellArg i.dataMount} --types nfs,nfs4 >/dev/null
        ${lib.optionalString (i.dataSource != null) ''
          test "$(${pkgs.util-linux}/bin/findmnt --noheadings --raw --mountpoint ${lib.escapeShellArg i.dataMount} --output SOURCE)" = ${lib.escapeShellArg i.dataSource}
        ''}
        test -r ${lib.escapeShellArg i.environmentFile}
        test -w ${lib.escapeShellArg i.dataMount}
        umask 077
        ${pkgs.coreutils}/bin/mkdir -p ${lib.escapeShellArg "${i.dataMount}/users"} ${lib.escapeShellArg "${i.dataMount}/metadata"}
      '';
      waitForOffice = pkgs.writeShellScript "${unit}-wait-for-office" ''
        ${pkgs.curl}/bin/curl \
          --fail \
          --silent \
          --show-error \
          --retry 60 \
          --retry-all-errors \
          --retry-delay 2 \
          --retry-max-time 180 \
          --connect-timeout 5 \
          ${lib.escapeShellArg "http://${officeProbeAddress}:${toString i.ports.office}/hosting/discovery"} >/dev/null
      '';
      start = pkgs.writeText "${unit}-start.sh" ''
        set -eu
        umask 077
        if [ ! -f /etc/opencloud/opencloud.yaml ]; then
          opencloud init
        fi
        exec opencloud server
      '';
      common = {
        inherit (i) uid;
        autoStart = true;
        unitConfig.StartLimitIntervalSec = 0;
        containerConfig.StopTimeout = 90;
        serviceConfig = {
          Restart = "on-failure";
          RestartSec = "5s";
          TimeoutStartSec = "900s";
          TimeoutStopSec = "120s";
        };
      };
      # Named volumes preserve image-provided ownership/content for the office's
      # multiple internal UIDs. They live in this user's local durable graphroot.
      officeVolumes = {
        keys = "/var/www/euro-office/Data";
        postgres = "/var/lib/postgresql";
        redis = "/var/lib/redis";
        rabbitmq = "/var/lib/rabbitmq";
        documents = "/var/lib/euro-office";
      };
    in
    {
      users.groups.${i.user}.gid = i.gid;
      users.users.${i.user} = {
        inherit (i) uid;
        group = i.user;
        isNormalUser = true;
        home = i.stateDir;
        createHome = false;
        shell = pkgs.shadow;
        linger = true;
        autoSubUidGidRange = true;
      };
      systemd.tmpfiles.rules = map (path: "d '${path}' 0700 ${i.user} ${i.user} - -") [
        i.stateDir
        "${i.stateDir}/config"
        "${i.stateDir}/data"
        i.cacheDir
        "${i.cacheDir}/thumbnails"
        "${i.cacheDir}/search"
      ];
      systemd.services."user@${toString i.uid}" = {
        overrideStrategy = "asDropin";
        # Drain containers while their user manager and transient network scopes
        # are still alive, before systemd begins tearing the session down.
        # Connect directly as this UID; --machine needs the system bus, which
        # can already be stopping during shutdown.
        serviceConfig.ExecStop = "${pkgs.coreutils}/bin/env XDG_RUNTIME_DIR=/run/user/${toString i.uid} ${pkgs.systemd}/bin/systemctl --user stop ${unit}.service ${officeUnit}.service";
        wants = [ "network-online.target" ];
        # Order shutdown before NFS unmount without requiring the mount to
        # succeed before the user manager starts its retrying Quadlets.
        after = [
          "network-online.target"
          "${utils.escapeSystemdPath i.dataMount}.mount"
        ];
        unitConfig.RequiresMountsFor = [
          i.stateDir
          i.cacheDir
        ];
      };
      virtualisation.quadlet = {
        networks.${unit} = {
          inherit (i) uid;
          networkConfig.NetworkName = unit;
        };
        volumes = lib.mapAttrs' (
          volume: _:
          lib.nameValuePair "${officeUnit}-${volume}" {
            inherit (i) uid;
            volumeConfig.VolumeName = "${officeUnit}-${volume}";
          }
        ) officeVolumes;
        containers.${unit} = lib.recursiveUpdate common {
          unitConfig = {
            Requires = [ "${officeUnit}.service" ];
            After = [ "${officeUnit}.service" ];
          };
          serviceConfig.ExecStartPre = [
            waitForOffice
            prepare
          ];
          containerConfig = {
            Image = i.image;
            ContainerName = unit;
            Network = "${unit}.network";
            UserNS = "keep-id:uid=1000,gid=1000";
            User = "1000:1000";
            Entrypoint = "/bin/sh";
            Exec = "/run/opencloud-start.sh";
            PublishPort = [ "${i.listenAddress}:${toString i.ports.app}:9200" ];
            EnvironmentFile = [ i.environmentFile ];
            Environment = envList (
              {
                OC_URL = "https://${i.domain}";
                OC_CONFIG_DIR = "/etc/opencloud";
                OC_BASE_DATA_PATH = "/var/lib/opencloud";
                OC_LOG_LEVEL = "info";
                OC_INSECURE = "false";
                OC_EXCLUDE_RUN_SERVICES = "idp";
                OC_ADD_RUN_SERVICES = "collaboration";
                OC_OIDC_ISSUER = i.oidc.issuer;
                PROXY_OIDC_REWRITE_WELLKNOWN = "true";
                WEB_OIDC_CLIENT_ID = i.oidc.clientId;
                PROXY_TLS = "false";
                PROXY_HTTP_ADDR = "0.0.0.0:9200";
                PROXY_ENABLE_BASIC_AUTH = "false";
                PROXY_AUTOPROVISION_ACCOUNTS = "true";
                PROXY_USER_OIDC_CLAIM = "sub";
                PROXY_AUTOPROVISION_CLAIM_USERNAME = "sub";
                PROXY_USER_CS3_CLAIM = "username";
                GRAPH_USERNAME_MATCH = "none";
                IDM_CREATE_DEMO_USERS = "false";
                STORAGE_USERS_DRIVER = "posix";
                STORAGE_USERS_POSIX_ROOT = "/data/users";
                STORAGE_SYSTEM_OC_ROOT = "/data/metadata";
                STORAGE_USERS_ID_CACHE_STORE = "nats-js-kv";
                FRONTEND_FULL_TEXT_SEARCH_ENABLED = "false";
                FRONTEND_CHECK_FOR_UPDATES = "false";
                OC_SHARING_PUBLIC_SHARE_MUST_HAVE_PASSWORD = "true";
                OC_SHARING_PUBLIC_WRITEABLE_SHARE_MUST_HAVE_PASSWORD = "true";
                PROXY_CSP_CONFIG_FILE_LOCATION = "/etc/opencloud/csp.yaml";
                COLLABORATION_WOPI_SRC = "https://${i.domain}";
                COLLABORATION_APP_NAME = "Euro-Office";
                COLLABORATION_APP_PRODUCT = "OnlyOffice";
                COLLABORATION_APP_ADDR = "https://${i.office.domain}";
                COLLABORATION_APP_ICON = "https://${i.office.domain}/web-apps/apps/documenteditor/main/resources/img/favicon.ico";
                COLLABORATION_APP_INSECURE = "false";
                COLLABORATION_CS3API_DATAGATEWAY_INSECURE = "false";
                # Upstream Euro Office compose disables WOPI proofs; retain that
                # compatibility setting explicitly, not TLS certificate bypasses.
                COLLABORATION_APP_PROOF_DISABLE = "true";
              }
              // i.environment
            );
            Volume = [
              "${i.stateDir}/config:/etc/opencloud"
              "${i.stateDir}/data:/var/lib/opencloud"
              "${i.dataMount}:/data"
              "${i.cacheDir}/thumbnails:/var/lib/opencloud/thumbnails"
              "${i.cacheDir}/search:/var/lib/opencloud/search"
              "${start}:/run/opencloud-start.sh:ro"
              "${csp}:/etc/opencloud/csp.yaml:ro"
              "${registry}:/etc/opencloud/app-registry.yaml:ro"
            ];
          };
        };
        containers.${officeUnit} = lib.recursiveUpdate common {
          serviceConfig.ExecStartPre = [ "${pkgs.coreutils}/bin/test -r ${i.office.environmentFile}" ];
          containerConfig = {
            Image = i.office.image;
            ContainerName = officeUnit;
            HostName = officeUnit;
            Network = "${unit}.network";
            PublishPort = [ "${i.listenAddress}:${toString i.ports.office}:80" ];
            EnvironmentFile = [ i.office.environmentFile ];
            Environment = envList (
              {
                WOPI_ENABLED = "true";
                USE_UNAUTHORIZED_STORAGE = "false";
              }
              // i.office.environment
            );
            Volume = lib.mapAttrsToList (volume: path: "${officeUnit}-${volume}.volume:${path}") officeVolumes;
            ShmSize = "256m";
          };
        };
      };
    };
in
{
  options.modules.services.opencloud.instances = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule instanceModule);
    default = { };
    description = "Independent rootless OpenCloud and Euro Office deployments. Ingress, NFS exports/mounts and runtime credentials are supplied by the host or guest configuration.";
  };
  config = lib.mkIf (instances != { }) {
    users = lib.mkMerge (lib.mapAttrsToList (name: i: (mkInstance name i).users) instances);
    systemd = lib.mkMerge (lib.mapAttrsToList (name: i: (mkInstance name i).systemd) instances);
    virtualisation.quadlet = lib.mkMerge (
      [ { enable = true; } ]
      ++ lib.mapAttrsToList (name: i: (mkInstance name i).virtualisation.quadlet) instances
    );
    assertions = [
      {
        assertion =
          unique (map (i: i.uid) values) && unique (map (i: i.gid) values) && unique (map (i: i.user) values);
        message = "OpenCloud instances require distinct users, UIDs and GIDs.";
      }
      {
        assertion = unique (
          lib.concatMap (i: [
            i.ports.app
            i.ports.office
          ]) values
        );
        message = "OpenCloud published ports must be distinct.";
      }
      {
        assertion = unique (
          lib.concatMap (i: [
            i.domain
            i.office.domain
          ]) values
        );
        message = "OpenCloud application and office domains must be distinct.";
      }
      {
        assertion =
          let
            paths = lib.concatMap (i: [
              i.stateDir
              i.cacheDir
              i.dataMount
            ]) values;
          in
          lib.all (
            a:
            builtins.match "/[a-zA-Z0-9_./-]+" a != null
            && lib.all (
              part:
              !(lib.elem part [
                ""
                "."
                ".."
              ])
            ) (builtins.tail (lib.splitString "/" a))
            && lib.all (b: a == b || !(lib.hasPrefix "${a}/" b)) paths
          ) paths
          && unique paths;
        message = "OpenCloud writable roots must be absolute, distinct and non-overlapping.";
      }
    ];
  };
}
