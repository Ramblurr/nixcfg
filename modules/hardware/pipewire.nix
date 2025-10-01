{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.hardware.pipewire;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;

  speaker-mute = pkgs.writeShellScriptBin "speaker-mute" ''
    #!${pkgs.runtimeShell}
    ${pkgs.alsa-utils}/bin/amixer -D pipewire set Master mute
  '';
  mic-mute = pkgs.writeShellScriptBin "mic-mute" ''
    #!${pkgs.runtimeShell}
    ${pkgs.alsa-utils}/bin/amixer -D pipewire set Capture nocap
  '';
  speaker-unmute = pkgs.writeShellScriptBin "speaker-unmute" ''
    #!${pkgs.runtimeShell}
    ${pkgs.alsa-utils}/bin/amixer -D pipewire set Master unmute
  '';
  mic-unmute = pkgs.writeShellScriptBin "mic-unmute" ''
    #!${pkgs.runtimeShell}
    ${pkgs.alsa-utils}/bin/amixer -D pipewire set Capture cap
  '';

  speaker-get-mute = pkgs.writeShellScriptBin "speaker-get-mute" ''
    #!${pkgs.runtimeShell}
    if ${pkgs.alsa-utils}/bin/amixer -D pipewire get Master | grep -q '\[off\]'; then
      echo "1"
    else
      echo "0"
    fi
  '';

  mic-get-mute = pkgs.writeShellScriptBin "mic-get-mute" ''
    #!${pkgs.runtimeShell}
    if ${pkgs.alsa-utils}/bin/amixer -D pipewire get Capture | grep -q '\[off\]'; then
      echo "1"
    else
      echo "0"
    fi
  '';
in
{
  options.modules.hardware.pipewire = {
    enable = lib.mkEnableOption "";
    denoise.enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {
    security.rtkit.enable = true;
    environment.systemPackages = with pkgs; [
      alsa-utils # ignore for now cross-compile problem
      pipewire
      wireplumber
      pulseaudio
      pulsemixer
      libcamera
      pw-viz
    ];
    programs.dconf.enable = true;
    systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      audio.enable = true;
      jack.enable = false;
      pulse.enable = true;
      wireplumber.enable = true;
    };
    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      {
        home.packages = [
          speaker-mute
          speaker-unmute
          speaker-get-mute
          mic-mute
          mic-unmute
          mic-get-mute
        ];

        xdg.configFile."pipewire/pipewire.conf.d/99-deepfilternet.conf" = lib.mkIf cfg.denoise.enable {
          text = builtins.toJSON {
            "context.properties" = {
              "link.max-buffers" = 16;
              "core.daemon" = true;
              "core.name" = "pipewire-0";
              "module.x11.bell" = false;
              "module.access" = true;
              "module.jackdbus-detect" = false;
            };

            "context.modules" = [
              {
                name = "libpipewire-module-filter-chain";
                args = {
                  "node.description" = "DeepFilter Noise Canceling source";
                  "media.name" = "DeepFilter Noise Canceling source";

                  "filter.graph" = {
                    nodes = [
                      {
                        type = "ladspa";
                        name = "DeepFilter Mono";
                        plugin = "${pkgs.deepfilternet}/lib/ladspa/libdeep_filter_ladspa.so";
                        label = "deep_filter_mono";
                        control = {
                          "Attenuation Limit (dB)" = 100;
                        };
                      }
                    ];
                  };

                  "audio.rate" = 48000;
                  "audio.position" = "[MONO]";

                  "capture.props"."node.passive" = true;
                  "playback.props"."media.class" = "Audio/Source";
                };
              }
            ];
          };
        };
      };
  };
}
