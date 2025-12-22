{
  config,
  ...
}:
let
  inherit (config.repo.secrets.global) domain email;
  grafanaDomain = "grafana.${domain.home}";

in
{
  sops.secrets = {
    pushover_api_token = {
      owner = "grafana";
    };
    pushover_user_key = {
      owner = "grafana";
    };
  };

  modules.services.ingress.virtualHosts.${grafanaDomain} = {
    acmeHost = domain.home;
    upstream = "http://unix:${config.services.grafana.settings.server.socket}";
    #forwardAuth = true;
  };
  systemd.services.nginx.serviceConfig.SupplementaryGroups = [ "grafana" ];
  modules.services.grafana.enable = true;
  modules.services.grafana.domain = grafanaDomain;
  services.grafana.provision = {
    enable = true;
    datasources.settings = {
      datasources = [
        {
          name = "Thanos";
          type = "prometheus";
          access = "proxy";
          url = "http://${config.services.thanos.query.http-address}";
        }
        #{
        #  name = "Loki";
        #  type = "loki";
        #  access = "proxy";
        #  url = "http://${monitorHostname}:${toString config.services.loki.configuration.server.http_listen_port}";
        #}
        #{
        #  name = "PostgreSQL";
        #  type = "postgres";
        #  access = "proxy";
        #  url = hosts.controller.config.networking.hostName;
        #  user = "blocky";
        #  postgresVersion = 1500;
        #  jsonData = {
        #    user = "blocky";
        #    database = "blocky";
        #    sslmode = "disable";
        #  };
        #}
      ];
    };
    dashboards.settings.providers = [
      {
        name = "Nodes";
        options.path = ./provisioning/nodes.json;
      }
      {
        name = "systemd Service Dashboard";
        options.path = ./provisioning/systemd.json;
      }
      {
        name = "ZFS Pool Status";
        options.path = ./provisioning/zfs.json;
      }
      {
        name = "UPS Status";
        options.path = ./provisioning/nut.json;
      }
      {
        name = "Smart Status";
        options.path = ./provisioning/smartctl2.json;
      }
      {
        name = "ntopng";
        options.path = ./provisioning/ntopng.json;
      }
      {
        name = "zrepl";
        options.path = ./provisioning/zrepl.json;
      }
    ];

    alerting = {
      contactPoints.settings = {
        apiVersion = 1;

        contactPoints = [
          {
            name = "Email";
            receivers = [
              {
                uid = "1";
                type = "email";
                settings = {
                  addresses = email.work;
                };
              }
            ];
          }
          {
            name = "Pushover";
            receivers = [
              {
                uid = "10";
                type = "pushover";
                settings = {
                  apiToken = "$__file{${config.sops.secrets.pushover_api_token.path}}";
                  userKey = "$__file{${config.sops.secrets.pushover_user_key.path}}";
                };
              }
            ];
          }
        ];
      };

      policies.settings = {
        apiVersion = 1;

        policies = [
          {
            orgId = 1;
            receiver = "Email";
            group_by = [
              "grafana_folder"
              "alertname"
            ];
            routes = [
              {
                receiver = "Pushover";
                object_matchers = [
                  [
                    "severity"
                    "="
                    "critical"
                  ]
                ];
              }
            ];
          }
        ];
      };

      rules.settings = {
        apiVersion = 1;
        deleteRules = [
        ];
        groups = [
          # TODO
        ];
      };
    };
  };
}
