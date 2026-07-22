{ inputs, pkgs }:

let
  stablePkgs = import inputs.nixpkgs-stable {
    system = pkgs.stdenv.hostPlatform.system;
    overlays = import ../pkgs/default.nix inputs;
  };
in
pkgs.testers.runNixOSTest {
  name = "linux-voice-assistant-output-only";

  nodes.machine = {
    imports = [ ../modules/services/linux-voice-assistant.nix ];

    networking.hostName = "lva-output-only-test";
    system.stateVersion = "26.05";

    services.pipewire = {
      enable = true;
      audio.enable = true;
      pulse.enable = false;
      systemWide = true;
      wireplumber.enable = true;
    };

    services.linux-voice-assistant = {
      enable = true;
      package = stablePkgs.linux-voice-assistant-unstable;
      user = "lva";
      group = "pipewire";
      name = "output-only-test";
      outputOnly = true;
      audioOutputDevice = "pipewire";
    };

    users.users.lva = {
      isSystemUser = true;
      group = "pipewire";
    };

    virtualisation.memorySize = 2048;
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("linux-voice-assistant.service")
    machine.wait_until_succeeds("systemctl is-active --quiet linux-voice-assistant.service")
    machine.wait_for_open_port(6053, timeout=120)
    machine.wait_for_open_port(6055, timeout=120)
    machine.sleep(3)
    machine.succeed("systemctl show linux-voice-assistant.service -p NRestarts --value | grep -x 0")
    machine.succeed("journalctl -u linux-voice-assistant.service | grep -F 'Server started'")
    machine.succeed("journalctl -u linux-voice-assistant.service | grep -F 'Peripheral API listening'")
    machine.fail("systemctl is-active --quiet pipewire-pulse.service")
    machine.fail("test -S /run/pulse/native")
    machine.fail("journalctl -u linux-voice-assistant.service | grep -F 'no soundcard'")
  '';
}
