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
      pipewireEnable ? true,
      pulseEnable ? pipewireEnable,
      dummyInput ? false,
      audioInputDevice ? null,
    }:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/services/linux-voice-assistant.nix
        (
          { options, ... }:
          {
            networking.hostName = "lva-test";
            system.stateVersion = "26.05";

            services.pipewire = {
              enable = pipewireEnable;
              pulse.enable = pulseEnable;
              inherit systemWide;
            };
            services.linux-voice-assistant = {
              enable = true;
              package = dummyPackage;
              user = "lva";
              group = "lva";
            }
            // lib.optionalAttrs (audioInputDevice != null) {
              inherit audioInputDevice;
            }
            // lib.optionalAttrs (options.services.linux-voice-assistant ? dummyAudioInput) {
              dummyAudioInput = {
                enable = dummyInput;
                deviceName = "lva-dummy-source";
              };
            };

            users.users.lva = {
              isSystemUser = true;
              group = "lva";
            };
            users.groups.lva = { };
          }
        )
      ];
    };

  userResult = mkConfig { };
  systemResult = mkConfig {
    systemWide = true;
    dummyInput = true;
  };
  explicitInputResult = mkConfig {
    audioInputDevice = "real-source";
  };
  conflictingInputResult = mkConfig {
    dummyInput = true;
    audioInputDevice = "real-source";
  };
  missingPipeWireResult = mkConfig {
    pipewireEnable = false;
    dummyInput = true;
  };
  missingPulseResult = mkConfig {
    pulseEnable = false;
    dummyInput = true;
  };
  systemWideWithoutPulseResult = mkConfig {
    systemWide = true;
    pulseEnable = false;
  };

  userService = userResult.config.systemd.services.linux-voice-assistant;
  systemService = systemResult.config.systemd.services.linux-voice-assistant;
  dummyObjects =
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
      systemResult.config;
  dummyObject = lib.head dummyObjects;
  failedAssertions =
    result:
    map (assertion: assertion.message) (
      lib.filter (assertion: !assertion.assertion) result.config.assertions
    );
in
assert userResult.options.services.linux-voice-assistant ? dummyAudioInput;
assert lib.hasInfix "export XDG_RUNTIME_DIR=/run/user/$UID" userService.script;
assert !(userService.environment ? PULSE_SERVER);
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
assert builtins.length dummyObjects == 1;
assert dummyObject.factory == "adapter";
assert dummyObject.args."factory.name" == "support.null-audio-sink";
assert dummyObject.args."node.name" == "lva-dummy-source";
assert dummyObject.args."media.class" == "Audio/Source/Virtual";
assert dummyObject.args."audio.position" == [ "MONO" ];
assert lib.hasInfix "--audio-input-device lva-dummy-source" systemService.script;
assert lib.any (message: lib.hasInfix "dummyAudioInput" message) (
  failedAssertions conflictingInputResult
);
assert lib.any (message: lib.hasInfix "PipeWire" message) (failedAssertions missingPipeWireResult);
assert lib.any (
  message: lib.hasInfix "dummyAudioInput requires services.pipewire.pulse.enable" message
) (failedAssertions missingPulseResult);
assert lib.any (
  message: lib.hasInfix "system-wide PipeWire requires services.pipewire.pulse.enable" message
) (failedAssertions systemWideWithoutPulseResult);
pkgs.runCommand "linux-voice-assistant-module-test" { } ''
  touch "$out"
''
