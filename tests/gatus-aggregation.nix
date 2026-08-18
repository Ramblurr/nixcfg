{ inputs, pkgs }:
let
  inherit (pkgs) lib;

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
              conditions = [ "[STATUS] == 200" ];
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
      conditions = [ "[STATUS] == 200" ];
    }
    {
      name = "paperless";
      group = "webapps";
      url = "https://paperless.example.test/api/schema/";
      interval = "5m";
      conditions = [ "[STATUS] == 200" ];
      alerts = [
        {
          type = "testing";
          "failure-threshold" = 3;
          description = "healthcheck failed";
        }
      ];
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
  };
assert
  credentialConsumer == {
    pushover-api-token = "op://home-ops-prod/pushover/pushover_api_token";
    pushover-user-key = "op://home-ops-prod/pushover/pushover_user_key";
  };
assert gatusEnvSetup.before == [ "gatus.service" ];
assert gatusEnvSetup.requiredBy == [ "gatus.service" ];
pkgs.runCommand "gatus-aggregation-test" { } ''
  touch "$out"
''
