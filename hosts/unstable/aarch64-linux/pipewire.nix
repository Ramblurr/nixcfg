{
  config,
  lib,
  pkgs,
  ...
}: {
  security.rtkit.enable = false;
  hardware.pulseaudio.enable = pkgs.lib.mkForce false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = false; # ?
    pulse.enable = false;
    jack.enable = false;
    wireplumber.enable = true;
    audio.enable = true;
    package = pkgs.pipewire.override {libcameraSupport = false;};
  };

  systemd.user.services = {
    pipewire.wantedBy = ["default.target"];
    wireplumber.wantedBy = ["default.target"];
  };
  environment.etc."pipewire/pipewire.conf.d/100-user.conf" = {
    text =
      builtins.toJSON
      {
        "context.modules" = [
          {
            name = "libpipewire-module-rt";
            args = {
              "nice.level" = 20;
              "rt.prio" = 88;
              "rtportal.enabled" = false;
              "rtkit.enabled" = false;
              "rlimits.enabled" = true;
            };
            flags = ["ifexists" "nofail"];
          }
          {
            name = "libpipewire-module-loopback";
            args = {
              "node.name" = "music-to-speakers-bridge";
              "node.description" = "music-to-speakers-bridge";
              "target.delay.sec" = 0;
              "capture.props" = {
                "node.target" = "alsa_input.platform-soc_sound.stereo-fallback";
              };
              "playback.props" = {
                "monitor.channel-volumes" = true;
                "media.role" = "Multimedia";
                #"device.intended-roles" = "Multimedia";
              };
            };
          }
        ];
      };
  };
  environment.etc."wireplumber/policy.lua.d/50-endpoints-config.lua".text = ''
    default_policy.policy.roles = {
      ["Capture"] = {
        ["alias"] = {  "Capture" },
        ["priority"] = 25,
        ["action.default"] = "cork",
        ["action.capture"] = "mix",
        ["media.class"] = "Audio/Source",
      },
      ["Multimedia"] = {
        ["alias"] = { "Movie", "Music", "Game" },
        ["priority"] = 25,
        ["action.default"] = "mix",
      },
      ["Speech"] = {
        ["priority"] = 60,
        ["action.default"] = "duck",
        ["action.Speech"] = "mix",
      }
    }

    default_policy.endpoints = {
      ["endpoint.capture"] = {
        ["media.class"] = "Audio/Source",
        ["role"] = "Capture",
      },
      ["endpoint.multimedia"] = {
        ["media.class"] = "Audio/Sink",
        ["role"] = "Multimedia",
      },
      ["endpoint.speech"] = {
        ["media.class"] = "Audio/Sink",
        ["role"] = "Speech",
      },
    }
  '';

  networking.firewall.allowedTCPPorts = [10001 10002 10003];
  environment.etc."pipewire/pipewire.conf.d/101-roc-recv.conf" = {
    text =
      builtins.toJSON
      {
        "context.modules" = [
          {
            name = "libpipewire-module-roc-source";
            args = {
              "local.ip" = "0.0.0.0";
              "resampler.profile" = "medium";
              "fec.code" = "rs8m";
              #"sess.latency.msec" = 60;
              "local.source.port" = 10001;
              "local.repair.port" = 10002;
              "source.name" = "ROC Source";
              "source.props" = {
                "node.name" = "roc-recv-source";
                "node.description" = "ROC Recv Source";
                "media.class" = "Audio/Source";
                #"role" = "Speech-High";
              };
            };
          }
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
  };

  environment.etc."wireplumber/main.lua.d/51-disable-builtin-rpi-audio.lua".text = ''
    rule = {
      matches = {
        {
          { "node.name", "equals", "alsa_output.platform-bcm2835_audio.stereo-fallback" },
        },
      },
      apply_properties = {
        ["device.disabled"] = true,
        ["node.description"] = "snd_rpi_builtin"
      },
    }

    table.insert(alsa_monitor.rules,rule)
  '';

  environment.etc."wireplumber/main.lua.d/51-rename-devices.lua".text = ''
    rule = {
      matches = {
        {
          { "node.name", "equals", "alsa_output.platform-soc_sound.stereo-fallback" },

        },
      },
      apply_properties = {
          ["node.description"] = "snd_rpi_hifiberry_dacplus_sink"
      },
    }

    table.insert(alsa_monitor.rules,rule)
    rule2 = {
      matches = {
        {
          { "node.name", "equals", "alsa_input.platform-soc_sound.stereo-fallback" },

        },
      },
      apply_properties = {
          ["node.description"] = "snd_rpi_hifiberry_dacplus_source"
      },
    }

    table.insert(alsa_monitor.rules,rule2)
  '';
}
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FL" "control.endpoint.multimedia:playback_FL"
# pw-link "alsa_input.platform-soc_sound.stereo-fallback:capture_FR" "control.endpoint.multimedia:playback_FR"
# pw-link "roc-recv-source:receive_FR" "control.endpoint.speech_high:playback_FR"
# pw-link "roc-recv-source:receive_FL" "control.endpoint.speech_high:playback_FL"

