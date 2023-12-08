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
  systemd.user.services = {
    # Required until wireplumber 0.5
    # Ref: https://gitlab.freedesktop.org/pipewire/wireplumber/-/issues/499
    analog-in-music-loopback = {
      enable = false;
      path = [
        pkgs.pipewire
        pkgs.wireplumber
        pkgs.coreutils
      ];
      description = "Loopback analog in to Multimedia Role";
      after = ["wireplumber.service"];
      wantedBy = ["wireplumber.service"];
      bindsTo = ["wireplumber.service"];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = 1;
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 10";
        ExecStart = ''
          ${pkgs.pipewire}/bin/pw-loopback --name analog-music-bridge --group analog-music-bridge --capture-props='[node.target=alsa_input.platform-soc_sound.stereo-fallback]' --playback-props='[media.role=Multimedia monitor.channel-volumes=true]'
        '';
        Type = "simple";
      };
    };
    digital-in-music-loopback = {
      enable = false;
      path = [
        pkgs.pipewire
        pkgs.wireplumber
        pkgs.coreutils
      ];
      description = "Loopback digital in to Multimedia Role";
      after = ["wireplumber.service"];
      wantedBy = ["wireplumber.service"];
      bindsTo = ["wireplumber.service"];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = 1;
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 10";
        ExecStart = ''
          ${pkgs.pipewire}/bin/pw-loopback --name digital-music-bridge --group digital-music-bridge  --capture-props='[node.target=alsa_input.usb-262a_UR23_USB_SPDIF_Rx-01.analog-stereo]' --playback-props='[media.role=Multimedia monitor.channel-volumes=true]'
        '';
        Type = "simple";
      };
    };
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
          # Disabled until wireplumber 0.5
          # Ref: https://gitlab.freedesktop.org/pipewire/wireplumber/-/issues/499
          # {
          #   name = "libpipewire-module-loopback";
          #   args = {
          #     "node.name" = "music-to-speakers-bridge";
          #     "node.description" = "music-to-speakers-bridge";
          #     "audio.position" = ["FL" "FR"];
          #     "target.delay.sec" = 0;
          #     "capture.props" = {
          #       "node.target" = "alsa_input.platform-soc_sound.stereo-fallback";
          #     };
          #     "playback.props" = {
          #
          #       "monitor.channel-volumes" = true;
          #       "media.role" = "Multimedia";
          #       #"device.intended-roles" = "Multimedia";
          #     };
          #   };
          # }
        ];
      };
  };
  environment.etc."wireplumber/policy.lua.d/50-endpoints-config.lua".text = ''
        default_policy.policy = {
          ["move"] = true;
          ["follow"] = true;
          ["duck.level"] = 0.2,
          ["roles"] = {
    --[[
    --]]
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
        }

        default_policy.endpoints = {
    --[[
    --]]
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
  networking.firewall.allowedUDPPorts = [10001 10002 10003];
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
              };
            };
          }
          #{
          #  "name" = "libpipewire-module-filter-chain";
          #  "args" = {
          #    "node.description" = "Noise Canceling source";
          #    "media.name" = "Noise Canceling source";
          #    "filter.graph" = {
          #      "nodes" = [
          #        {
          #          "type" = "ladspa";
          #          "name" = "rnnoise";
          #          "plugin" = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
          #          "label" = "noise_suppressor_stereo";
          #          "control" = {
          #            "VAD Threshold (%)" = 95.0;
          #            "VAD Grace Period (ms)" = 200;
          #            "Retroactive VAD Grace (ms)" = 0;
          #          };
          #        }
          #      ];
          #    };
          #    "capture.props" = {
          #      "audio.position" = ["FL" "FR"];
          #      "node.name" = "effect_input.rnnoise";
          #      "node.passive" = true;
          #    };
          #    "playback.props" = {
          #      "audio.position" = ["FL" "FR"];
          #      "node.name" = "effect_output.rnnoise";
          #      "media.class" = "Audio/Source";
          #      "media.role" = "Speech";
          #    };
          #  };
          #}
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

    rule3 = {
      matches = {
        {
          { "node.name", "equals", "alsa_input.usb-262a_UR23_USB_SPDIF_Rx-01.analog-stereo"},
        },
      },
      apply_properties = {
          ["node.description"] = "snd_usb_ur23_spdif_source",
          ["audio.rate"] = 96000,
          --["api.alsa.use_hw"] = true,
          --["api.alsa.fix_rate"] = true,
          ["audio.format"] = "S32LE"
      },
    }
    --table.insert(alsa_monitor.rules,rule3)

    rule4 = {
      matches = {
        {
          { "node.name", "equals", "alsa_input.usb-ANKER_Anker_PowerConf_S330_ACCUDP1D20502303-00.analog-stereo"},
        },
      },
      apply_properties = {
          ["node.description"] = "snd_usb_anker_powerconf_s330_source"
      },
    }
    table.insert(alsa_monitor.rules,rule4)
  '';
}
# Link the music system input manually:
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FL" "control.endpoint.multimedia:playback_FL"
# pw-link "alsa_input.platform-soc_sound.stereo-fallback:capture_FR" "control.endpoint.multimedia:playback_FR"
#
# To hear the mic input on the speakers:
# pw-link "roc-recv-source:receive_FR" "control.endpoint.speech:playback_FR"
# pw-link "roc-recv-source:receive_FL" "control.endpoint.speech:playback_FL"
#
# pw-link -d "roc-recv-source:receive_FL" "control.endpoint.capture:playback_FL"
# pw-link -d "roc-recv-source:receive_FR" "control.endpoint.capture:playback_FR"
# pw-link -d "roc-recv-source:receive_FR" "input.music-to-speakers-bridge:input_FR"
# pw-link -d "roc-recv-source:receive_FL" "input.music-to-speakers-bridge:input_FL"
# This was required to get the mic input into ovos
# pw-link "roc-recv-source:receive_FR" "ALSA Capture:input_FR"
# pw-link "roc-recv-source:receive_FL" "ALSA Capture:input_FL"
#    or this one for noise reduction (only use one or the other)
# pw-link  "effect_output.rnnoise:capture_FR" "ALSA Capture:input_FR"
# pw-link  "effect_output.rnnoise:capture_FL" "ALSA Capture:input_FL"
# after restart
# pw-link -d "roc-recv-source:receive_FR" "input.music-to-speakers-bridge:input_FR"
# pw-link -d "roc-recv-source:receive_FL" "input.music-to-speakers-bridge:input_FL"
# pw-link -d "control.endpoint.capture:monitor_FL" "ALSA Capture:input_FL"
# pw-link -d "control.endpoint.capture:monitor_FR" "ALSA Capture:input_FR"
# pw-link "roc-recv-source:receive_FR" "ALSA Capture:input_FR"
# pw-link "roc-recv-source:receive_FL" "ALSA Capture:input_FL"
# wpctl set-volume 60 1.0
#
# again
# # physical mic to loopback
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FL" "input.music-to-speakers-bridge:input_FL"
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FR" "input.music-to-speakers-bridge:input_FR"
# loopback to multimedia endpoint
# pw-link output.music-to-speakers-bridge:output_FL "control.endpoint.multimedia:playback_FL"
# pw-link output.music-to-speakers-bridge:output_FR "control.endpoint.multimedia:playback_FR"
# remove capture to bridge
# pw-link -d "control.endpoint.capture:monitor_FL" "input.music-to-speakers-bridge:input_FL"
# pw-link -d "control.endpoint.capture:monitor_FR" "input.music-to-speakers-bridge:input_FR"
# pw-link -d "roc-recv-source:receive_FL" "control.endpoint.capture:playback_FL"
# pw-link -d "roc-recv-source:receive_FR" "control.endpoint.capture:playback_FR"
# pw-link -d "roc-recv-source:receive_FL" "input.music-to-speakers-bridge:input_FL"
# pw-link -d "roc-recv-source:receive_FR" "input.music-to-speakers-bridge:input_FR"
# pw-link "roc-recv-source:receive_FL" "ALSA Capture:input_FL"
# pw-link "roc-recv-source:receive_FR" "ALSA Capture:input_FR"
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FL" "input.music-to-speakers-bridge:input_FL"
# pw-link  "alsa_input.platform-soc_sound.stereo-fallback:capture_FR" "input.music-to-speakers-bridge:input_FR"
# pw-link -d "control.endpoint.capture:monitor_FL" "ALSA Capture:input_FL"
# pw-link -d "control.endpoint.capture:monitor_FR" "ALSA Capture:input_FR"

