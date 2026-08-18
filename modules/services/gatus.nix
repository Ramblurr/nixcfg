{
  config,
  lib,
  nodes ? { },
  ...
}:

let
  cfg = config.modules.services.gatus;
  environmentFile = "/run/gatus-env/gatus.env";
  onepassword = config.modules.services.onepassword-systemd-credentials;
  nodeConfigs =
    let
      configs = map (node: node.config) (builtins.attrValues nodes);
    in
    if configs == [ ] then [ config ] else configs;
  collectEndpoints =
    name: lib.unique (lib.concatMap (nodeConfig: nodeConfig.site.gatus.${name}) nodeConfigs);
  stateDirActual = "/var/lib/private/gatus";
  stateDirEffective = "/var/lib/gatus";
in
{
  options.modules.services.gatus = {
    enable = lib.mkEnableOption "Gatus";
    domain = lib.mkOption {
      type = lib.types.nonEmptyStr;
      example = "status.example.com";
      description = "Public hostname for Gatus";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 10022;
      description = "Local HTTP port for Gatus";
    };
    allowedRemoteIPs = lib.mkOption {
      type = lib.types.listOf lib.types.nonEmptyStr;
      default = [ ];
      description = "Peer addresses and CIDRs allowed through the Caddy route";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Gatus Pushover credentials require the 1Password systemd credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.gatus-env-setup = {
      borgmatic-external-endpoint-token = "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
      pushover-api-token = "op://home-ops-prod/pushover/pushover_api_token";
      pushover-user-key = "op://home-ops-prod/pushover/pushover_user_key";
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/gatus".mountpoint = stateDirActual;
    };

    services.gatus = {
      enable = true;
      inherit environmentFile;
      settings = {
        alerting.pushover = {
          application-token = "$PUSHOVER_API_TOKEN";
          user-key = "$PUSHOVER_USER_KEY";
          default-alert = {
            description = "health-check failed";
            failure-threshold = 3;
            minimum-reminder-interval = "1h";
            send-on-resolved = true;
            success-threshold = 2;
          };
        };
        endpoints = collectEndpoints "endpoints";
        external-endpoints = collectEndpoints "externalEndpoints";
        web.port = cfg.port;
        storage = {
          type = "sqlite";
          path = "${stateDirEffective}/data.db";
        };
      };
    };

    systemd.services.gatus-env-setup = {
      description = "Prepare Gatus environment from 1Password credentials";
      before = [ "gatus.service" ];
      requiredBy = [ "gatus.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        RuntimeDirectory = "gatus-env";
        UMask = "0077";
      };
      script = ''
        {
          printf 'BORGMATIC_GATUS_TOKEN=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/borgmatic-external-endpoint-token")"
          printf 'GATUS_EXTERNAL_TOKEN=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/borgmatic-external-endpoint-token")"
          printf 'PUSHOVER_API_TOKEN=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/pushover-api-token")"
          printf 'PUSHOVER_USER_KEY=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/pushover-user-key")"
        } > ${environmentFile}
      '';
    };

    systemd.services.gatus.unitConfig.RequiresMountsFor = [ stateDirActual ];

    modules.services.caddy.routes.gatus = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.port}";
      inherit (cfg) allowedRemoteIPs;
    };
  };
}
