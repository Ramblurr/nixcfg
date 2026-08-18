{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  groups = import ../modules/site/gatus-groups.nix;

  testOptions = {
    options = {
      repo.secrets = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
      modules.services.caddy.routes = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      modules.services.onepassword-systemd-credentials = {
        enable = lib.mkEnableOption "test credential provider";
        consumers = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = { };
        };
      };
      modules.zfs.datasets.properties = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
    };
  };

  commonModules = [
    ../modules/site
    testOptions
  ];

  hosts = inputs.self.lib.nixcfg.mkHosts {
    debord = {
      enableDefaultModules = false;
      hostPath = ./fixtures/gatus-host;
      hostExtraModules = commonModules ++ [
        ../modules/services/gatus.nix
        {
          networking.hostName = "debord";
          system.stateVersion = "26.05";
          modules.services.onepassword-systemd-credentials.enable = true;
          modules.services.gatus = {
            enable = true;
            domain = "status.example.test";
          };
          site.gatus.endpoints = [
            {
              name = "dewey";
              group = "hosts";
              url = "https://dewey.example.test";
            }
          ];
        }
      ];
    };

    dewey = {
      enableDefaultModules = false;
      hostPath = ./fixtures/gatus-host;
      hostExtraModules = commonModules ++ [
        ../modules/services/paperless.nix
        {
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
          repo.secrets.global.nodes.mali.dataCIDR = "192.0.2.1";
          modules.services.onepassword-systemd-credentials.enable = true;
          modules.services.paperless = {
            enable = true;
            domain = "paperless.example.test";
            ports.http = 28981;
            nfsShare = "paperless";
            user = {
              name = "paperless";
              uid = 991;
            };
            group = {
              name = "paperless";
              gid = 991;
            };
          };
          site.gatus.endpoints = [
            {
              name = "dewey";
              group = "hosts";
              url = "https://dewey.example.test";
            }
          ];
          site.gatus.externalEndpoints = [
            {
              name = "paperless-worker";
              token = "test-token";
            }
          ];
        }
      ];
    };

    disabled = {
      enableDefaultModules = false;
      hostPath = ./fixtures/gatus-host;
      hostExtraModules = commonModules ++ [
        ../modules/services/paperless.nix
        {
          networking.hostName = "disabled";
          system.stateVersion = "26.05";
        }
      ];
    };
  };

  endpoints = hosts.debord.config.services.gatus.settings.endpoints;
  externalEndpoints = hosts.debord.config.services.gatus.settings."external-endpoints";
  pushover = hosts.debord.config.services.gatus.settings.alerting.pushover;
  credentialConsumer =
    hosts.debord.config.modules.services.onepassword-systemd-credentials.consumers.gatus-env-setup;
  gatusEnvSetup = hosts.debord.config.systemd.services.gatus-env-setup;
in
assert
  endpoints == [
    {
      name = "dewey";
      group = "hosts";
      url = "https://dewey.example.test";
      interval = "5m";
      conditions = [ "[STATUS] == 200" ];
      alerts = [ { type = "pushover"; } ];
    }
    {
      name = "Paperless";
      group = groups.home;
      url = "https://paperless.example.test/api/schema/";
      interval = "5m";
      conditions = [ "[STATUS] == 200" ];
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert
  externalEndpoints == [
    {
      name = "paperless-worker";
      token = "test-token";
    }
  ];
assert
  pushover == {
    "application-token" = "$PUSHOVER_API_TOKEN";
    "user-key" = "$PUSHOVER_USER_KEY";
    default-alert = {
      description = "health-check failed";
      failure-threshold = 3;
      minimum-reminder-interval = "1h";
      send-on-resolved = true;
      success-threshold = 2;
    };
  };
assert
  credentialConsumer == {
    borgmatic-external-endpoint-token = "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
    pushover-api-token = "op://home-ops-prod/pushover/pushover_api_token";
    pushover-user-key = "op://home-ops-prod/pushover/pushover_user_key";
  };
assert gatusEnvSetup.before == [ "gatus.service" ];
assert gatusEnvSetup.requiredBy == [ "gatus.service" ];
assert lib.hasInfix "BORGMATIC_GATUS_TOKEN=%s" gatusEnvSetup.script;
assert lib.hasInfix "GATUS_EXTERNAL_TOKEN=%s" gatusEnvSetup.script;
pkgs.runCommand "gatus-aggregation-test" { } ''
  touch "$out"
''
