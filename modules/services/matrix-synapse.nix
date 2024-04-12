{
  options,
  config,
  lib,
  pkgs,
  inputs,
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
      slidingSync = lib.mkOption {
        type = lib.types.port;
        description = "The port to use for the sliding sync service";
      };
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
    slidingSyncUser = lib.mkOption { type = lib.types.unspecified; };
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
      "~ ^/(client/|_matrix/client/unstable/org.matrix.msc3575/sync)" = {
        priority = 800;
        proxyPass = "http://${config.services.matrix-sliding-sync.settings.SYNCV3_BINDADDR}";
      };
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
    };
    users.users.${cfg.slidingSyncUser.name} = {
      uid = cfg.slidingSyncUser.uid;
      isSystemUser = true;
      group = cfg.group.name;
    };

    users.groups.matrix-synapse = {
      gid = lib.mkForce cfg.group.gid;
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

    sops.secrets."signingKey" = {
      sopsFile = ../../configs/home-ops/matrix-synapse.sops.yaml;
      owner = cfg.user.name;
      group = cfg.group.name;
      mode = "400";
    };

    sops.secrets."slidingSyncEnv" = {
      sopsFile = ../../configs/home-ops/matrix-synapse.sops.yaml;
      owner = cfg.slidingSyncUser.name;
      mode = "400";
    };

    services.matrix-sliding-sync = {
      enable = true;
      createDatabase = false;
      environmentFile = config.sops.secrets."slidingSyncEnv".path;
      settings = {
        SYNCV3_BINDADDR = "127.0.0.1:${toString cfg.ports.slidingSync}";
        SYNCV3_DB = "postgresql:///matrix-sliding-sync?host=/run/postgresql-matrix-synapse";
        SYNCV3_SERVER = "http://127.0.0.1:${toString cfg.ports.http}";
      };
    };

    systemd.services.matrix-sliding-sync.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = cfg.slidingSyncUser.name;
    };

    services.matrix-synapse = {
      enable = true;
      withJemalloc = true;
      dataDir = synapseDataDir;
      settings = {
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
        extra_well_known_client_content = {
          "org.matrix.msc3575.proxy" = {
            "url" = "https://${cfg.domain}";
          };
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
      };
      #extraConfigFiles = [ config.sops.secrets."matrixSynapse/config".path ];
    };
  };
}
