{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.site.gatus;
  heartbeatPackage = pkgs.callPackage ../../pkgs/gatus-heartbeat.nix { };
  hostName = config.networking.hostName;
  gatusUrl = "https://status.${config.repo.secrets.global.domain.home}";
  systemdHeartbeats = lib.filterAttrs (_: heartbeat: heartbeat.service != null) cfg.heartbeats;
  endpointName = heartbeat: "${heartbeat.name} (${hostName})";
  reporterCommand =
    heartbeat:
    lib.concatStringsSep " " [
      (lib.getExe heartbeatPackage)
      "systemd"
      "--url"
      (lib.escapeShellArg gatusUrl)
      "--group"
      (lib.escapeShellArg heartbeat.group)
      "--name"
      (lib.escapeShellArg (endpointName heartbeat))
    ];
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
        type = lib.types.enum [
          "Infrastructure & Operations"
          "Home & Personal"
          "Work & Collaboration"
          "Media & Library"
        ];
        description = "Domain-oriented Gatus group";
      };
      interval = lib.mkOption {
        type = lib.types.nonEmptyStr;
        description = "Maximum expected interval between successful job runs";
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
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = heartbeat.interval;
      alerts = [ { type = "pushover"; } ];
    }) cfg.heartbeats;

    systemd.services = lib.mapAttrs' (
      _: heartbeat:
      lib.nameValuePair heartbeat.service {
        serviceConfig = {
          ExecStopPost = reporterCommand heartbeat;
          EnvironmentFile = lib.mkIf (
            cfg.heartbeatToken.environmentFile != null
          ) cfg.heartbeatToken.environmentFile;
        };
      }
    ) systemdHeartbeats;
  };
}
