{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.pipewire;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;

  json = pkgs.formats.json {};

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
          "audio.position" = ["FL" "FR"];
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
in {
  imports = [inputs.nix-gaming.nixosModules.default];
  options.modules.hardware.pipewire = {
    enable = mkBoolOpt false;
    denoise.enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    security.rtkit.enable = true;
    sound.enable = true;
    #nixpkgs.config.pulseaudio = true;
    hardware.pulseaudio.enable = pkgs.lib.mkForce false;

    environment.systemPackages = with pkgs; [
      alsa-utils #ignore for now cross-compile problem
      pipewire
      wireplumber
      pulseaudio
      pulsemixer
      libcamera
      pw-viz
    ];
    programs.dconf.enable = true;
    systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true; # ?
      pulse.enable = true;
      jack.enable = false;
      wireplumber.enable = true;
      lowLatency.enable = true;
      audio.enable = true;
    };

    home-manager.users."${username}" = {pkgs, ...} @ hm:
      mkIf cfg.denoise.enable {
        xdg.configFile."pipewire/pipewire.conf.d/99-input-denoising.conf" = {
          source = json.generate "99-input-denoising.conf" pw_rnnoise_config;
        };
      };
  };
}