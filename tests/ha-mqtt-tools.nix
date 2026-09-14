{ pkgs }:
let
  desktop =
    name:
    pkgs.writeShellScriptBin name ''
      exec ${pkgs.python3}/bin/python3 ${./fixtures/ha-mqtt-desktop.py} ${name} "$@"
    '';
  speakers = import ../modules/hardware/speaker-actions.nix {
    pkgs = pkgs // {
      alsa-utils = desktop "amixer";
    };
  };
in
pkgs.buildEnv {
  name = "ha-mqtt-test-tools";
  paths = speakers ++ [
    (desktop "dunstify")
    (desktop "systemctl")
    (desktop "pactl")
    pkgs.babashka
    (import ../modules/desktop/services/ha-mqtt/mqttx.nix { inherit pkgs; })
    pkgs.mosquitto
    pkgs.python3
  ];
}
