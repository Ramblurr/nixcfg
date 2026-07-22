{ inputs, pkgs }:

let
  lib = inputs.nixpkgs.lib;
  dummyPackage = pkgs.writeShellApplication {
    name = "linux-voice-assistant";
    text = "exit 0";
  };
  mkConfig =
    {
      systemWide ? false,
      pulseEnable ? true,
      outputOnly ? false,
      audioInputDevice ? null,
    }:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/services/linux-voice-assistant.nix
        {
          networking.hostName = "lva-test";
          system.stateVersion = "26.05";

          services.pipewire = {
            enable = true;
            pulse.enable = pulseEnable;
            inherit systemWide;
          };
          services.linux-voice-assistant = {
            enable = true;
            package = dummyPackage;
            user = "lva";
            group = "lva";
            inherit outputOnly;
          }
          // lib.optionalAttrs (audioInputDevice != null) {
            inherit audioInputDevice;
          };

          users.users.lva = {
            isSystemUser = true;
            group = "lva";
          };
          users.groups.lva = { };
        }
      ];
    };

  userResult = mkConfig { };
  systemResult = mkConfig {
    systemWide = true;
  };
  outputOnlyResult = mkConfig {
    systemWide = true;
    outputOnly = true;
  };
  outputOnlyWithoutPulseResult = mkConfig {
    systemWide = true;
    pulseEnable = false;
    outputOnly = true;
  };
  explicitInputResult = mkConfig {
    audioInputDevice = "real-source";
  };
  conflictingInputResult = mkConfig {
    outputOnly = true;
    audioInputDevice = "real-source";
  };
  systemWideWithoutPulseResult = mkConfig {
    systemWide = true;
    pulseEnable = false;
  };

  userService = userResult.config.systemd.services.linux-voice-assistant;
  systemService = systemResult.config.systemd.services.linux-voice-assistant;
  outputOnlyService = outputOnlyResult.config.systemd.services.linux-voice-assistant;
  outputOnlyWithoutPulseService =
    outputOnlyWithoutPulseResult.config.systemd.services.linux-voice-assistant;
  outputOnlyObjects =
    lib.attrByPath
      [
        "services"
        "pipewire"
        "extraConfig"
        "pipewire"
        "99-linux-voice-assistant-dummy-input"
        "context.objects"
      ]
      [ ]
      outputOnlyResult.config;
  failedAssertions =
    result:
    map (assertion: assertion.message) (
      lib.filter (assertion: !assertion.assertion) result.config.assertions
    );
in
assert userResult.options.services.linux-voice-assistant ? outputOnly;
assert lib.hasInfix "export XDG_RUNTIME_DIR=/run/user/$UID" userService.script;
assert !(userService.environment ? PULSE_SERVER);
assert !(lib.hasInfix "--output-only" userService.script);
assert !(lib.hasInfix "--audio-input-device" userService.script);
assert lib.hasInfix "--audio-input-device real-source"
  explicitInputResult.config.systemd.services.linux-voice-assistant.script;
assert systemService.environment.PULSE_SERVER == "unix:/run/pulse/native";
assert systemService.environment.PIPEWIRE_RUNTIME_DIR == "/run/pipewire";
assert !(lib.hasInfix "export XDG_RUNTIME_DIR=/run/user/$UID" systemService.script);
assert lib.all (unit: lib.elem unit systemService.after) [
  "pipewire.service"
  "pipewire-pulse.service"
  "wireplumber.service"
];
assert lib.all (unit: lib.elem unit systemService.requires) [
  "pipewire.service"
  "pipewire-pulse.service"
];
assert lib.hasInfix "--output-only" outputOnlyService.script;
assert !(lib.hasInfix "--audio-input-device" outputOnlyService.script);
assert builtins.length outputOnlyObjects == 0;
assert outputOnlyWithoutPulseService.environment.PIPEWIRE_RUNTIME_DIR == "/run/pipewire";
assert !(outputOnlyWithoutPulseService.environment ? PULSE_SERVER);
assert !(lib.elem "pipewire-pulse.service" outputOnlyWithoutPulseService.after);
assert !(lib.elem "pipewire-pulse.service" outputOnlyWithoutPulseService.requires);
assert
  !(lib.any (
    message: lib.hasInfix "system-wide PipeWire requires services.pipewire.pulse.enable" message
  ) (failedAssertions outputOnlyWithoutPulseResult));
assert lib.any (message: lib.hasInfix "outputOnly conflicts with audioInputDevice" message) (
  failedAssertions conflictingInputResult
);
assert lib.any (
  message: lib.hasInfix "system-wide PipeWire requires services.pipewire.pulse.enable" message
) (failedAssertions systemWideWithoutPulseResult);
pkgs.runCommand "linux-voice-assistant-module-test" { } ''
  touch "$out"
''
