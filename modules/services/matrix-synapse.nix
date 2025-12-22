{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.matrix-synapse;
  service = "matrix-synapse";
  synapseDataDir = "${cfg.dataDir}/synapse";
in
{
  options.modules.services.matrix-synapse = {
    enable = lib.mkEnableOption "matrix-synapse";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "matrix.example.com";
      description = "The domain to use";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/${service}";
      description = "The directory to store data in";
    };

    serverName = lib.mkOption {
      type = lib.types.str;
      example = "example.com";
      description = "The matrix server domain to use";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
      };
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
    bridgesGroup = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        message = "The user name and group must be matrix-synapse, because that's that the nixos module hard codes";
        assertion = cfg.user.name == "matrix-synapse" && cfg.group.name == "matrix-synapse";
      }
    ];
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      http3.enable = false;
    };
    services.nginx.virtualHosts.${cfg.domain}.locations = {
      "~ ^(/_matrix|/_synapse/client)" = {
        proxyPass = "http://127.0.0.1:${toString cfg.ports.http}";
        extraConfig = ''
          client_max_body_size 200M;
        '';
      };
      "/.well-known/matrix/server".extraConfig =
        let
          server = {
            "m.server" = "${cfg.domain}:443";
          };
        in
        ''
          add_header Content-Type application/json;
          return 200 '${builtins.toJSON server}';
        '';
    };

    # the nixos module hardcodes the user matrix-synapse
    users.users.matrix-synapse = {
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      createHome = lib.mkForce false;
      extraGroups = [ cfg.bridgesGroup.name ];
    };
    users.groups.matrix-synapse = {
      gid = lib.mkForce cfg.group.gid;
    };

    users.groups.${cfg.bridgesGroup.name} = {
      gid = lib.mkForce cfg.bridgesGroup.gid;
    };

    modules.zfs.datasets.properties = {
      "tank/svc/${service}"."mountpoint" = cfg.dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${synapseDataDir}' 751 ${cfg.user.name} ${cfg.group.name} - -"
      "d '${synapseDataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "Z '${synapseDataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d '${cfg.dataDir}/pg' 750 postgres postgres - -"
      "d '/run/postgresql-matrix-synapse' 755 postgres postgres - -"
      "Z '${cfg.dataDir}/pg' 750 postgres postgres - -"
    ];

    systemd.services.${config.services.matrix-synapse.serviceUnit} = {
      after = [ "postgresql.service" ];
      unitConfig = {
        RequiresMountsFor = [ cfg.dataDir ];
      };
    };

    sops.secrets."doublepuppet.yaml" = {
      sopsFile = ../../configs/home-ops/matrix-synapse.sops.yaml;
      owner = cfg.user.name;
      group = cfg.bridgesGroup.name;
      mode = "440";
    };

    sops.secrets."signingKey" = {
      sopsFile = ../../configs/home-ops/matrix-synapse.sops.yaml;
      owner = cfg.user.name;
      group = cfg.group.name;
      mode = "400";
    };

    systemd.services.matrix-synapse.environment = {
      SYNAPSE_CACHE_FACTOR = "1.0";
      LimitNOFILE = "4096";
    };
    services.matrix-synapse = {
      enable = true;
      dataDir = synapseDataDir;
      settings = {
        app_service_config_files = [ config.sops.secrets."doublepuppet.yaml".path ];

        public_baseurl = "https://${cfg.domain}";
        report_stats = true;
        enable_metrics = true;
        enable_registration = false;
        dynamic_thumbnails = true;
        url_preview_enabled = false;
        registration_requires_token = true;
        server_name = cfg.serverName;
        signing_key_path = config.sops.secrets."signingKey".path;
        suppress_key_server_warning = true;
        enable_search = true;
        allow_public_rooms_over_federation = true;
        redaction_retention_period = 1;
        max_upload_size = "200M";
        extra_well_known_client_content = {
          "org.matrix.msc3575.proxy" = {
            "url" = "https://${cfg.domain}";
          };
        };
        experimental_features = {
          # Room summary api
          msc3266_enabled = true;
          # Removing account data
          msc3391_enabled = true;
          # Thread notifications
          msc3773_enabled = true;
          # Remotely toggle push notifications for another client
          msc3881_enabled = true;
          # Remotely silence local notifications
          msc3890_enabled = true;
        };

        rc_admin_redaction = {
          per_second = 1000;
          burst_count = 10000;
        };
        database = {
          name = "psycopg2";
          args.user = cfg.user.name;
          args.database = cfg.user.name;
          args.host = "/run/postgresql-matrix-synapse";
        };
        listeners = [
          {
            port = cfg.ports.http;
            bind_addresses = [ "127.0.0.1" ];
            type = "http";
            tls = false;
            x_forwarded = true;
            resources = [
              {
                names = [
                  "client"
                  "federation"
                ];
                compress = true;
              }
            ];
          }
        ];
        log_config = builtins.toFile "synapse-log-config.yaml" ''
          version: 1
          formatters:
            journal_fmt:
              format: '%(name)s: [%(request)s] %(message)s'
          filters:
            context:
              (): synapse.util.logcontext.LoggingContextFilter
              request: ""
          handlers:
            journal:
              class: systemd.journal.JournalHandler
              formatter: journal_fmt
              filters: [context]
              SYSLOG_IDENTIFIER: synapse
          disable_existing_loggers: True
          loggers:
            synapse:
              level: WARN
            synapse.storage.SQL:
              level: WARN
          root:
            level: WARN
            handlers: [journal]
        '';
      };
      #extraConfigFiles = [ config.sops.secrets."matrixSynapse/config".path ];
    };
  };
}
