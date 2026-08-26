{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.site.gatus;
  groups = import ./gatus-groups.nix;
  heartbeatPackage = pkgs.callPackage ../../pkgs/gatus-heartbeat.nix { };
  hostName = config.networking.hostName;
  gatusUrl = "https://status.${config.repo.secrets.global.domain.home}";
  systemdHeartbeats = lib.filterAttrs (_: heartbeat: heartbeat.service != null) cfg.heartbeats;
  endpointName = heartbeat: "${heartbeat.name} (${hostName})";
  reporterCommand =
    heartbeat:
    lib.concatStringsSep " " (
      [
        (lib.getExe heartbeatPackage)
        "report"
        "--success"
        "true"
        "--url"
        (lib.escapeShellArg gatusUrl)
        "--group"
        (lib.escapeShellArg heartbeat.group)
        "--name"
        (lib.escapeShellArg (endpointName heartbeat))
      ]
      ++ lib.optionals (cfg.heartbeatToken.environmentFile == null) [
        "--token-file"
        "%d/gatus-token"
      ]
    );
  heartbeatType = lib.types.submodule {
    options = {
      service = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Systemd service to report, without the .service suffix; null for a native adapter";
      };
      name = lib.mkOption {
        type = lib.types.nonEmptyStr;
        description = "Human-readable Gatus endpoint name before the hostname suffix";
      };
      group = lib.mkOption {
        type = lib.types.enum (builtins.attrValues groups);
        description = "Domain-oriented Gatus group";
      };
      interval = lib.mkOption {
        type = lib.types.strMatching "^([0-9]+(ms|s|m|h))+$";
        description = "Maximum expected interval using Go duration units (h, m, s, or ms)";
      };
    };
  };
in
{
  options.site.gatus = {
    heartbeats = lib.mkOption {
      type = lib.types.attrsOf heartbeatType;
      default = { };
      description = "Recurring jobs reported to Gatus external endpoints";
    };
    heartbeatToken = {
      available = lib.mkOption {
        type = lib.types.bool;
        default = cfg.heartbeatToken.environmentFile != null;
        internal = true;
        description = "Whether this host has a configured recurring-job heartbeat token source";
      };
      gatusEnvironmentVariable = lib.mkOption {
        type = lib.types.strMatching "^[A-Z][A-Z0-9_]*$";
        default = "GATUS_EXTERNAL_TOKEN";
        description = "Gatus process environment variable containing the token that validates this host's external endpoints";
      };
      onepasswordReference = lib.mkOption {
        type = lib.types.strMatching "^op://.+";
        default = "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
        description = "Shared external-heartbeat token reference for 1Password-enabled hosts";
      };
      environmentFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Environment file containing GATUS_EXTERNAL_TOKEN on hosts without the 1Password provider";
      };
    };
  };

  config = lib.mkIf (cfg.heartbeats != { }) {
    assertions = [
      {
        assertion =
          let
            services = map (heartbeat: heartbeat.service) (builtins.attrValues systemdHeartbeats);
          in
          builtins.length services == builtins.length (lib.unique services);
        message = "Each systemd service may have only one site.gatus heartbeat declaration.";
      }
    ];

    site.gatus.externalEndpoints = lib.mapAttrsToList (_: heartbeat: {
      name = endpointName heartbeat;
      inherit (heartbeat) group;
      token = "$" + cfg.heartbeatToken.gatusEnvironmentVariable;
      heartbeat.interval = heartbeat.interval;
      alerts = [ { type = "pushover"; } ];
    }) cfg.heartbeats;

    systemd.services = lib.mapAttrs' (
      _: heartbeat:
      lib.nameValuePair heartbeat.service {
        serviceConfig = {
          ExecStartPost = reporterCommand heartbeat;
          EnvironmentFile = lib.mkIf (
            cfg.heartbeatToken.environmentFile != null
          ) cfg.heartbeatToken.environmentFile;
        };
      }
    ) systemdHeartbeats;
  };
}
