{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.hardware.pipewire;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  imports = [ ];
  options.modules.hardware.pipewire = {
    enable = lib.mkEnableOption "";
    denoise.enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
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
      mkIf cfg.denoise.enable {
        xdg.configFile."pipewire/pipewire.conf.d/99-deepfilternet.conf" = {
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
