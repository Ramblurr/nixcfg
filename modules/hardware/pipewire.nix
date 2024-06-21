{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.pipewire;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;

  json = pkgs.formats.json { };

  pw_rnnoise_config = {
    "context.modules" = [
      {
        "name" = "libpipewire-module-filter-chain";
        "args" = {
          "node.description" = "Noise Canceling source";
          "media.name" = "Noise Canceling source";
          "filter.graph" = {
            "nodes" = [
              {
                "type" = "ladspa";
                "name" = "rnnoise";
                "plugin" = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                "label" = "noise_suppressor_stereo";
                "control" = {
                  "VAD Threshold (%)" = 95.0;
                  "VAD Grace Period (ms)" = 200;
                  "Retroactive VAD Grace (ms)" = 0;
                };
              }
            ];
          };
          "audio.position" = [
            "FL"
            "FR"
          ];
          "capture.props" = {
            "node.name" = "effect_input.rnnoise";
            "node.passive" = true;
          };
          "playback.props" = {
            "node.name" = "effect_output.rnnoise";
            "media.class" = "Audio/Source";
          };
        };
      }
    ];
  };
in
{
  imports = [ ];
  options.modules.hardware.pipewire = {
    enable = mkBoolOpt false;
    denoise.enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    security.rtkit.enable = true;

    # Vehemently do not use plain alsa or pulse audio, pipewire has its own alsa and pulse compat support
    sound.enable = pkgs.lib.mkForce false;
    hardware.pulseaudio.enable = pkgs.lib.mkForce false;

    environment.systemPackages = with pkgs; [
      alsa-utils # ignore for now cross-compile problem
      pipewire
      wireplumber
      pulseaudio
      pulsemixer
      libcamera
      #pw-viz # TODO(disabled 2023-12-01 compilation error) https://github.com/NixOS/nixpkgs/issues/268450
    ];
    programs.dconf.enable = true;
    systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = false; # ? lets see if this breaks something
      pulse.enable = true;
      jack.enable = false;
      wireplumber.enable = true;
      audio.enable = true;
    };

    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      mkIf cfg.denoise.enable {
        xdg.configFile."pipewire/pipewire.conf.d/99-input-denoising.conf" = {
          source = json.generate "99-input-denoising.conf" pw_rnnoise_config;
        };
      };
  };
}
