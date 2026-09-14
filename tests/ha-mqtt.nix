{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  evaluate =
    settings:
    (lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/desktop/services/ha-mqtt
        ({ lib, ... }: {
          options.modules.users.primaryUser.username = lib.mkOption {
            type = lib.types.str;
            default = "desktop";
          };
          options.modules.hardware.pipewire.enable = lib.mkOption {
            type = lib.types.bool;
            default = true;
          };
          config = {
            networking.hostName = "test-host";
            networking.firewall.allowedTCPPorts = [ 1234 ];
            system.stateVersion = "26.05";
            services.pipewire = {
              enable = true;
              pulse.enable = true;
            };
            modules.desktop.services.ha-mqtt = settings;
          };
        })
      ];
    }).config;
  base = {
    enable = true;
    mqtt.passwordFile = "/run/secrets/mqtt-password";
    mqtt.username = "desktop";
  };
  enabled = evaluate base;
  disabled = evaluate { };
  overridden = evaluate (
    base
    // {
      topicPrefix = "custom/desktop";
      mqtt = base.mqtt // {
        tls.enable = true;
      };
    }
  );
  service = enabled.systemd.user.services.ha-mqtt;
  invalid =
    prefix:
    lib.any (a: !a.assertion && lib.hasInfix "topicPrefix" a.message)
      (evaluate (base // { topicPrefix = prefix; })).assertions;
  tools = import ./ha-mqtt-tools.nix { inherit pkgs; };
in
assert enabled.modules.desktop.services.ha-mqtt.topicPrefix == "ha-mqtt/test-host";
assert overridden.modules.desktop.services.ha-mqtt.topicPrefix == "custom/desktop";
assert overridden.modules.desktop.services.ha-mqtt.mqtt.port == 8883;
assert
  enabled.modules.desktop.services.ha-mqtt.shutdown == {
    enable = false;
    dryRun = true;
    gracePeriodMs = 60000;
  };
assert !(disabled.systemd.user.services ? ha-mqtt);
assert !(enabled.systemd.user.services ? ha-shutdown);
assert !(enabled.systemd.services ? ha-mqtt);
assert enabled.networking.firewall.allowedTCPPorts == [ 1234 ];
assert service.unitConfig.ConditionUser == "desktop";
assert service.partOf == [ "graphical-session.target" ];
assert service.serviceConfig.KillMode == "control-group";
assert service.environment.XDG_RUNTIME_DIR == "%t";
assert lib.hasInfix "mqttx-cli" service.environment.PATH;
assert lib.hasInfix "speaker-get-mute" service.environment.PATH;
assert lib.hasInfix "pulseaudio" service.environment.PATH;
assert lib.elem "pipewire-pulse.service" service.after;
assert lib.all invalid [
  ""
  "prefix/"
  "prefix/+"
  "prefix/#"
];
assert !(lib.hasInfix "ha-shutdown" (builtins.readFile ../modules/default.nix));
assert !(builtins.pathExists ../modules/desktop/services/shutdown.py);
pkgs.runCommand "ha-mqtt-tests"
  {
    nativeBuildInputs = [ tools ];
    HA_MQTT_BRIDGE = ../modules/desktop/services/ha-mqtt/bridge.clj;
  }
  ''
    export HOME="$TMPDIR"
    python3 ${./ha-mqtt.py} -v
    touch "$out"
  ''
