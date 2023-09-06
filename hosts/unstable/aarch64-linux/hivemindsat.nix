{
  config,
  lib,
  pkgs,
  ...
}: {
  users.defaultUserShell = pkgs.bash;
  users.mutableUsers = false;
  users.groups = {
    ovos = {
      gid = 1000;
      name = "ovos";
    };
    spi = {};
    gpio = {};
  };
  services.udev.extraRules = ''
    SUBSYSTEM=="spidev", KERNEL=="spidev0.0", GROUP="spi", MODE="0660"
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
  '';
  users.users = {
    ovos = {
      uid = 1000;
      home = "/home/ovos";
      name = "ovos";
      group = "ovos";
      isNormalUser = true;
      extraGroups = ["wheel" "audio" "spi" "gpio"];
    };
  };
  security.pam.loginLimits = [
    {
      domain = "@audio";
      item = "memlock";
      type = "-";
      value = "4194304";
    }
    {
      domain = "@audio";
      item = "rtprio";
      type = "-";
      value = "95";
    }
    {
      domain = "@audio";
      item = "nice";
      type = "-";
      value = "-19";
    }
    {
      domain = "@audio";
      item = "nofile";
      type = "soft";
      value = "99999";
    }
    {
      domain = "@audio";
      item = "nofile";
      type = "hard";
      value = "99999";
    }
  ];

  nix = {
    gc = lib.mkDefault {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  environment.systemPackages = with pkgs; [
    # dev packages, remove in the future
    jq
    python311
    nix-prefetch-docker

    # actual packages we need for ovos
    git
    alsa-utils
    libraspberrypi
    raspberrypi-eeprom

    # friendly sysadmin tools
    htop
    ncdu
    bash
    vim
    curl
    wget
    jless
  ];

  documentation = {
    enable = true;
    doc.enable = false;
    man.enable = true;
    dev.enable = false;
  };
  system.activationScripts = {
    enableLingering = ''
      # remove all existing lingering users
      rm -r /var/lib/systemd/linger
      mkdir /var/lib/systemd/linger
      # enable for the subset of declared users
      touch /var/lib/systemd/linger/ovos
    '';
  };
  sops.secrets."hivemindSatEnv" = {
    sopsFile = ./secrets.sops.yaml;
    restartUnits = [];
    owner = "ovos";
    group = "ovos";
  };
  systemd.tmpfiles.rules = [
    "d /home/ovos/config 0755 ovos ovos"
    "d /home/ovos/share 0755 ovos ovos"
    "d /home/ovos/config/mycroft 0755 ovos ovos"
    "d /home/ovos/config/hivemind 0755 ovos ovos"
    "d /home/ovos/share/mycroft 0755 ovos ovos"
    "d /home/ovos/share/hivemind 0755 ovos ovos"
    "d /home/ovos/cache 0755 ovos ovos"
    "d /home/ovos/cache/ovos_listener_records 0755 ovos ovos"
    "d /home/ovos/cache/ovos_models 0755 ovos ovos"
    "d /home/ovos/cache/ovos_vosk 0755 ovos ovos"
    "d /home/ovos/cache/ovos_tts_cache 0755 ovos ovos"
  ];
  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers = {
    container-name = {
      image = "ghcr.io/ramblurr/hivemind-voice-sat:alpha";
      autoStart = true;
      volumes = [
        "/tmp/mycroft:/tmp/mycroft:z"
        "/home/ovos/cache/ovos_listener_records:/home/ovos/.local/share/mycroft/listener:Z"
        "/home/ovos/cache/ovos_models:/home/ovos/.local/share/precise-lite:z"
        "/home/ovos/cache/ovos_vosk:/home/ovos/.local/share/vosk:z"
        "/home/ovos/cache/ovos_tts_cache:/home/ovos/.cache/mycroft:z"
        "/run/user/1000/bus:/run/user/1000/bus:ro"
        "/run/user/1000/pipewire-0:/run/user/1000/pipewire-0:ro"
        "/home/ovos/config/hivemind:/home/ovos/.config/hivemind:z"
        "/home/ovos/config/mycroft:/home/ovos/.config/mycroft:z"
        "/home/ovos/share/hivemind:/home/ovos/.local/share/hivemind:z"
        "/home/ovos/share/mycroft:/home/ovos/.local/share/mycroft:z"
      ];
      extraOptions = [
        "--device /dev/snd"
      ];
      user = "ovos:ovos";
      environmentFiles = [
        config.sops.secrets.hivemindSatEnv.path
      ];
      environment = {
        TZ = config.time.timeZone;
        DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/1000/bus";
        XDG_RUNTIME_DIR = "/run/user/1000";
      };
    };
  };
}
