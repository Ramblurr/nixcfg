{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.linux-voice-assistant;

  inherit (lib)
    escapeShellArgs
    getExe
    literalExpression
    mkOption
    mkEnableOption
    mkIf
    mkPackageOption
    optional
    optionals
    types
    ;

  finalPackage = cfg.package;
  #finalPackage = cfg.package.overridePythonAttrs (oldAttrs: {
  #  dependencies =
  #    oldAttrs.dependencies
  #    # for audio enhancements like auto-gain, noise suppression
  #    ++ cfg.package.optional-dependencies.webrtc
  #    # vad is currently optional, because it is broken on aarch64-linux
  #    ++ optionals cfg.vad.enable cfg.package.optional-dependencies.silerovad;
  #});
in

{
  meta.buildDocsInSandbox = false;

  options.services.linux-voice-assistant = with types; {
    enable = mkEnableOption "Linux Voice Assistant (successor to Wyoming Satellite)";

    package = mkPackageOption pkgs "linux-voice-assistant-unstable" { };

    user = mkOption {
      type = str;
      example = "alice";
      description = ''
        User to run linux-voice-assistant under.
      '';
    };

    group = mkOption {
      type = str;
      default = "users";
      description = ''
        Group to run linux-voice-assistant under.
      '';
    };

    host = mkOption {
      type = str;
      default = "0.0.0.0";
      description = ''
        Address for ESPHome server.
      '';
    };

    port = mkOption {
      type = port;
      default = 6053;
      description = ''
        Port for ESPHome server.
      '';
    };

    openFirewall = mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to open the firewall port for the ESPHome server.
      '';
    };

    name = mkOption {
      type = str;
      default = config.networking.hostName;
      defaultText = literalExpression ''
        config.networking.hostName
      '';
      description = ''
        Name of the satellite.
      '';
    };

    audioInputDevice = mkOption {
      type = nullOr str;
      default = null;
      example = "plughw:CARD=seeed2micvoicec,DEV=0";
      description = ''
        Soundcard name for input device.
        Use `linux-voice-assistant --list-input-devices` to find available devices.
      '';
    };

    audioInputBlockSize = mkOption {
      type = nullOr int;
      default = null;
      description = ''
        Block size for audio input.
      '';
    };

    audioOutputDevice = mkOption {
      type = nullOr str;
      default = null;
      example = "alsa/plughw:CARD=seeed2micvoicec,DEV=0";
      description = ''
        MPV name for output device.
        Use `linux-voice-assistant --list-output-devices` to find available devices.
      '';
    };

    wakeWordDir = mkOption {
      type = nullOr path;
      default = null;
      description = ''
        Directory with wake word models (.tflite) and configs (.json).
      '';
    };

    wakeModel = mkOption {
      type = nullOr str;
      default = null;
      example = "ok_nabu";
      description = ''
        ID of active wake model.
      '';
    };

    stopModel = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        ID of stop model.
      '';
    };

    stateDir = mkOption {
      type = path;
      default = "/var/lib/linux-voice-assistant";
      description = ''
        Directory for writable state (downloads, preferences).
      '';
    };

    downloadDir = mkOption {
      type = nullOr path;
      default = null;
      defaultText = literalExpression "\${cfg.stateDir}/downloads";
      description = ''
        Directory to download custom wake word models, etc.
        Defaults to a subdirectory of stateDir.
      '';
    };

    refractorySeconds = mkOption {
      type = nullOr (either int float);
      default = null;
      description = ''
        Seconds before wake word can be activated again.
      '';
    };

    wakeupSound = mkOption {
      type = nullOr path;
      default = null;
      description = ''
        Path to audio file to play when wake word is detected.
      '';
    };

    timerFinishedSound = mkOption {
      type = nullOr path;
      default = null;
      description = ''
        Path to audio file to play when a timer finishes.
      '';
    };

    preferencesFile = mkOption {
      type = nullOr path;
      default = null;
      defaultText = literalExpression "\${cfg.stateDir}/preferences.json";
      description = ''
        Path to preferences file.
        Defaults to a file in stateDir.
      '';
    };

    debug = mkOption {
      type = bool;
      default = false;
      description = ''
        Enable debug logging.
      '';
    };

    extraArgs = mkOption {
      type = listOf str;
      default = [ ];
      description = ''
        Extra arguments to pass to the executable.

        Check `linux-voice-assistant --help` for possible options.
      '';
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.services."linux-voice-assistant" = {
      description = "Linux Voice Assistant";
      after = [
        "network-online.target"
        "sound.target"
      ];
      wants = [
        "network-online.target"
        "sound.target"
      ];
      wantedBy = [
        "multi-user.target"
      ];
      path = with pkgs; [
        alsa-utils
      ];
      script =
        let
          optionalParam =
            param: argument:
            optionals (argument != null) [
              param
              (toString argument)
            ];

          # Use explicit values or fall back to stateDir-based defaults
          downloadDir = if cfg.downloadDir != null then cfg.downloadDir else "${cfg.stateDir}/downloads";
          preferencesFile =
            if cfg.preferencesFile != null then cfg.preferencesFile else "${cfg.stateDir}/preferences.json";
        in
        ''
          export XDG_RUNTIME_DIR=/run/user/$UID
          ${escapeShellArgs (
            [
              (getExe finalPackage)
              "--name"
              cfg.name
              "--host"
              cfg.host
              "--port"
              (toString cfg.port)
              "--download-dir"
              downloadDir
              "--preferences-file"
              preferencesFile
            ]
            ++ optionalParam "--audio-input-device" cfg.audioInputDevice
            ++ optionalParam "--audio-input-block-size" cfg.audioInputBlockSize
            ++ optionalParam "--audio-output-device" cfg.audioOutputDevice
            ++ optionalParam "--wake-word-dir" cfg.wakeWordDir
            ++ optionalParam "--wake-model" cfg.wakeModel
            ++ optionalParam "--stop-model" cfg.stopModel
            ++ optionalParam "--refractory-seconds" cfg.refractorySeconds
            ++ optionalParam "--wakeup-sound" cfg.wakeupSound
            ++ optionalParam "--timer-finished-sound" cfg.timerFinishedSound
            ++ optional cfg.debug "--debug"
            ++ cfg.extraArgs
          )}
        '';
      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        StateDirectory = "linux-voice-assistant";
        StateDirectoryMode = "0750";
        LockPersonality = true;
        MemoryDenyWriteExecute = false; # TensorFlow Lite requires executable memory
        PrivateDevices = false; # Needs access to audio devices
        PrivateUsers = true;
        ProtectHome = false; # Needs access to pipewire socket
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ProtectProc = "invisible";
        ProcSubset = "all"; # TensorFlow needs /proc/cpuinfo
        Restart = "always";
        RestartSec = "5";
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SupplementaryGroups = [
          "audio"
          "pipewire"
        ];
        UMask = "0077";
      };
    };
  };
}
