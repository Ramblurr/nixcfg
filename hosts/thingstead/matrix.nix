{
  config,
  ...
}:

let
  service = "matrix-synapse";
  serverName = "moot.land";
  publicHost = "matrix.${serverName}";
  httpPort = 8008;
  dataDir = "/var/lib/${service}";
  synapseDataDir = "${dataDir}/synapse";
  postgresRootDataDir = "/var/lib/postgresql";
  postgresDataDir = "${postgresRootDataDir}/${config.services.postgresql.package.psqlSchema}";
  publicBaseUrl = "https://${publicHost}";
  maxUploadSize = "200M";
in
{
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/${service}"."mountpoint" = dataDir;
  };

  sops.secrets."matrixSynapse/macaroon_secret_key" = {
    owner = service;
    group = service;
    mode = "400";
  };

  sops.secrets."matrixSynapse/signing_key" = {
    owner = service;
    group = service;
    mode = "400";
  };

  sops.secrets."matrixSynapse/registration_shared_secret" = {
    owner = service;
    group = service;
    mode = "400";
  };

  systemd.tmpfiles.rules = [
    "z '${synapseDataDir}' 751 matrix-synapse matrix-synapse - -"
    "d '${synapseDataDir}' 750 matrix-synapse matrix-synapse - -"
    "Z '${synapseDataDir}' 750 matrix-synapse matrix-synapse - -"
    "d '${postgresDataDir}' 700 postgres postgres - -"
    "Z '${postgresDataDir}' 700 postgres postgres - -"
  ];

  services.postgresql = {
    enable = true;
    dataDir = postgresDataDir;
  };

  systemd.services.postgresql.unitConfig.RequiresMountsFor = [ postgresRootDataDir ];

  systemd.services.matrix-synapse-db-setup = {
    description = "Create PostgreSQL role and database for Synapse";
    after = [ "postgresql.service" ];
    before = [ config.services.matrix-synapse.serviceUnit ];
    requiredBy = [ config.services.matrix-synapse.serviceUnit ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
    };
    path = [ config.services.postgresql.package ];
    script = ''
      set -eu

      role_exists=$(psql -tAc "SELECT 1 FROM pg_roles WHERE rolname = '${service}'")
      if [ "$role_exists" != "1" ]; then
        psql -v ON_ERROR_STOP=1 -c 'CREATE ROLE "${service}" LOGIN'
      fi

      db_exists=$(psql -tAc "SELECT 1 FROM pg_database WHERE datname = '${service}'")
      if [ "$db_exists" != "1" ]; then
        psql -v ON_ERROR_STOP=1 -c 'CREATE DATABASE "${service}" WITH OWNER "${service}" TEMPLATE template0 LC_COLLATE = "C" LC_CTYPE = "C"'
      fi
    '';
  };

  systemd.services.${config.services.matrix-synapse.serviceUnit} = {
    after = [ "postgresql.service" ];
    unitConfig.RequiresMountsFor = [
      dataDir
      postgresRootDataDir
    ];
  };

  services.matrix-synapse = {
    enable = true;
    dataDir = synapseDataDir;
    settings = {
      public_baseurl = publicBaseUrl;
      report_stats = true;
      enable_metrics = true;
      enable_registration = true;
      registration_requires_token = true;
      registration_shared_secret_path =
        config.sops.secrets."matrixSynapse/registration_shared_secret".path;
      macaroon_secret_key_path = config.sops.secrets."matrixSynapse/macaroon_secret_key".path;
      dynamic_thumbnails = true;
      url_preview_enabled = false;
      server_name = serverName;
      signing_key_path = config.sops.secrets."matrixSynapse/signing_key".path;
      suppress_key_server_warning = true;
      enable_search = true;
      allow_public_rooms_over_federation = true;
      ip_range_whitelist = [
        "10.9.0.0/16"
        "100.117.130.101/32"
      ];
      redaction_retention_period = 1;
      max_upload_size = maxUploadSize;
      extra_well_known_client_content = {
        "org.matrix.msc3575.proxy" = {
          "url" = publicBaseUrl;
        };
      };
      experimental_features = {
        msc3266_enabled = true;
        msc3391_enabled = true;
        msc3773_enabled = true;
        msc3881_enabled = true;
        msc3890_enabled = true;
      };
      rc_admin_redaction = {
        per_second = 1000;
        burst_count = 10000;
      };
      database = {
        name = "psycopg2";
        args = {
          user = service;
          database = service;
          host = "/run/postgresql";
        };
      };
      listeners = [
        {
          port = httpPort;
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
            level: INFO
          synapse.storage.SQL:
            level: WARN
        root:
          level: WARN
          handlers: [journal]
      '';
    };
  };
}
