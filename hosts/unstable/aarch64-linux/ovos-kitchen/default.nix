{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ovos-kitchen";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ../../../home.nix
    inputs.nixos-ovos.nixosModules.ovos
    inputs.nixos-raspberrypi.nixosModules.base
    inputs.nixos-raspberrypi.nixosModules.hardware
    inputs.nixos-raspberrypi.inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "feedb33df4cc4cbf8e64e91cf837d8b2";

  raspberry-pi.hardware.hifiberry-dacplusadc.enable = true;
  raspberry-pi.hardware.platform.type = "rpi4";
  services.timesyncd.enable = true;
  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    users.enable = true;
    users.primaryUser = {
      username = ramblurr.username;
      uid = 1001;
      name = ramblurr.name;
      homeDirectory = ramblurr.homeDirectory;
      signingKey = ramblurr.signingKey;
      email = ramblurr.email;
      passwordSecretKey = ramblurr.passwordSecretKey;
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
      ];
    };
  };

  ovos.password.enable = true;
  ovos.gui.enable = false;
  ovos.sshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== cardno:000500006944";
  services.ovos.services = {
    skill_roon = {
      enable = true;
      image = "ghcr.io/ramblurr/ovos-skill-roon";
      tag = "dev";
      requires = ["ovos_core"];
    };
  };
  environment.etc."pipewire/pipewire.conf.d/101-roc-recv.conf" = {
    text =
      builtins.toJSON
      {
        "context.modules" = [
          {
            name = "libpipewire-module-roc-source";
            args = {
              "local.ip" = "10.9.6.27";
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
# pw-link "roc-recv-source:receive_FR" "control.endpoint.speech:playback_FR"
# pw-link "roc-recv-source:receive_FL" "control.endpoint.speech:playback_FL"
# pw-link "output.music-to-speakers-bridge:output_FR" "control.endpoint.multimedia:playback_FR"
# pw-link "output.music-to-speakers-bridge:output_FL" "control.endpoint.multimedia:playback_FL"
# pw-link -d roc-recv-source:receive_FL input.music-to-speakers-bridge:input_FL
# pw-link -d roc-recv-source:receive_FR input.music-to-speakers-bridge:input_FR
# sudo podman images | awk '(NR>1) && ($2!~/none/) {print $1":"$2}' |grep docker.io | xargs -L1 sudo podman pull

