{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.desktop.services.ha-mqtt;
  username = config.modules.users.primaryUser.username;
  settings = pkgs.writeText "ha-mqtt.json" (
    builtins.toJSON {
      inherit (cfg) topicPrefix mqtt shutdown;
    }
  );
  tools = [
    pkgs.babashka
    (import ./mqttx.nix { inherit pkgs; })
    pkgs.dunst
    pkgs.systemd
  ]
  ++ (import ../../../hardware/speaker-actions.nix { inherit pkgs; });
in
{
  options.modules.desktop.services.ha-mqtt = {
    enable = lib.mkEnableOption "host-scoped Home Assistant MQTT desktop actions";
    topicPrefix = lib.mkOption {
      type = lib.types.str;
      default = "ha-mqtt/${config.networking.hostName}";
      description = "Exact MQTT topic prefix, without wildcards, NUL, or a trailing slash.";
    };
    mqtt = {
      host = lib.mkOption {
        type = lib.types.str;
        default = "localhost";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = if cfg.mqtt.tls.enable then 8883 else 1883;
      };
      tls.enable = lib.mkEnableOption "verified MQTT TLS";
      tls.caFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional CA certificate path. TLS verification is never disabled.";
      };
      username = lib.mkOption {
        type = lib.types.str;
        default = "";
      };
      passwordFile = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Runtime plaintext password file, owned by the primary user with mode 0400 or 0600. Never a store path. One final newline is removed.";
      };
    };
    credentialUnits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "User-manager units that deliver credentials; required and ordered before this service. System-manager secret delivery must finish before the desktop session starts.";
    };
    shutdown = {
      enable = lib.mkEnableOption "cancellable MQTT shutdown (separate opt-in)";
      gracePeriodMs = lib.mkOption {
        type = lib.types.ints.between 1000 3600000;
        default = 60000;
        description = "Desktop cancellation grace period in milliseconds. Only validated notification expiry permits poweroff.";
      };
      dryRun = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Log validated shutdown expiry without requesting poweroff. Disable only after harmless validation and operational approval.";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        # Nix strings cannot contain NUL; the runtime also validates JSON prefixes.
        assertion =
          cfg.topicPrefix != ""
          && !(lib.hasSuffix "/" cfg.topicPrefix)
          && !(lib.hasInfix "+" cfg.topicPrefix)
          && !(lib.hasInfix "#" cfg.topicPrefix);
        message = "ha-mqtt.topicPrefix must be nonempty, without wildcards, NUL, or a trailing slash.";
      }
      {
        assertion =
          lib.hasPrefix "/" cfg.mqtt.passwordFile && !(lib.hasPrefix "/nix/store/" cfg.mqtt.passwordFile);
        message = "ha-mqtt.mqtt.passwordFile must be an absolute runtime path outside the Nix store.";
      }
      {
        assertion = config.modules.hardware.pipewire.enable;
        message = "ha-mqtt requires the PipeWire desktop module.";
      }
    ];
    systemd.user.services.ha-mqtt = {
      description = "Home Assistant MQTT desktop actions";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      after = [
        "graphical-session.target"
        "pipewire.service"
      ]
      ++ cfg.credentialUnits;
      requires = cfg.credentialUnits;
      unitConfig.ConditionUser = username;
      path = tools;
      environment = {
        XDG_RUNTIME_DIR = "%t";
        DBUS_SESSION_BUS_ADDRESS = "unix:path=%t/bus";
      };
      serviceConfig = {
        ExecStart = "${pkgs.babashka}/bin/bb ${./bridge.clj} ${settings}";
        Restart = "always";
        RestartSec = "3s";
        KillMode = "control-group";
        TimeoutStopSec = "10s";
        UMask = "0077";
        RuntimeDirectory = "ha-mqtt";
        RuntimeDirectoryMode = "0700";
        Environment = "TMPDIR=%t/ha-mqtt";
      };
    };
  };
}
