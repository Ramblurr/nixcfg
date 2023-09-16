{
  config,
  lib,
  pkgs,
  ...
}: {
  services.timesyncd.enable = true;
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
  users.users = {
    ovos = {
      uid = 1000;
      home = "/home/ovos";
      name = "ovos";
      group = "ovos";
      isNormalUser = true;
      extraGroups = ["wheel" "audio" "spi" "gpio" "input" "plugdev"];
      shell = pkgs.zsh;
    };
  };
  home-manager.users."ovos" = {
    home.stateVersion = "23.05";
    home.homeDirectory = "/home/ovos";
    systemd.user.startServices = true;
    programs = {
      home-manager.enable = true;
      zsh = {
        enable = true;
        autocd = true;
        enableCompletion = true;
        envExtra = ''
          export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"
          export XDG_RUNTIME_DIR="/run/user/1000"
        '';
      };
    };
    home.sessionVariables = {
      DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/1000/bus";
      XDG_RUNTIME_DIR = "/run/user/1000";
      EDITOR = "vim";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="spidev", KERNEL=="spidev0.0", GROUP="spi", MODE="0660"
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
  '';
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

  virtualisation.podman = {
    enable = true;
  };

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
    owner = "ovos";
    group = "ovos";
  };
  systemd.tmpfiles.rules = [
    "d /home/ovos/config 0750 ovos ovos"
    "d /home/ovos/share 0750 ovos ovos"
    "d /home/ovos/config/mycroft 0750 ovos ovos"
    "d /home/ovos/config/hivemind 0750 ovos ovos"
    "d /home/ovos/share/mycroft 0750 ovos ovos"
    "d /home/ovos/share/hivemind 0750 ovos ovos"
    "d /home/ovos/cache 0750 ovos ovos"
    "d /home/ovos/cache/ovos_listener_records 0750 ovos ovos"
    "d /home/ovos/cache/ovos_models 0750 ovos ovos"
    "d /home/ovos/cache/ovos_vosk 0750 ovos ovos"
    "d /home/ovos/cache/ovos_tts_cache 0750 ovos ovos"
    "d /home/ovos/cache/ovos_nltk 0750 ovos ovos"
    "d /tmp/mycroft 0750 ovos ovos"
  ];

  systemd.user.services.podman-auto-update-self = {
    wants = ["network-online.target"];
    after = ["network-online.target"];
    path = [
      "/run/wrappers"
      pkgs.coreutils
      config.virtualisation.podman.package
      pkgs.shadow
    ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "podman auto-update";
      ExecStartPost = "podman image prune -f";
    };
  };

  systemd.user.timers.podman-auto-update-self = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };

  systemd.user.services = {
    hivemind-sat = let
      image = "ghcr.io/ramblurr/hivemind-voice-sat-dev";
      tag = "rolling";
      xdgRuntimeDir = "/run/user/1000";
      pullPolicy = "missing";
      podman = "${config.virtualisation.podman.package}/bin/podman";
    in {
      path = [
        "/run/wrappers"
        pkgs.coreutils
        config.virtualisation.podman.package
        pkgs.shadow
      ];
      description = "Podman hivemind-sat";
      after = ["network-online.target"];
      wantedBy = ["default.target"];
      partOf = ["pipewire.service"];

      enable = true;
      unitConfig = {
        "RequiresMountsFor" = "%t/containers";
      };
      serviceConfig = {
        Environment = "PODMAN_SYSTEMD_UNIT=%n";
        Restart = "on-failure";
        TimeoutStopSec = 70;
        TimeoutStartSec = "infinity";
        RestartSec = 5;
        ExecStart = ''
          ${podman} run \
            --name=hivemind-sat \
            --cidfile=%t/%n.ctr-id \
            --cgroups=no-conmon \
            --rm \
            --sdnotify=conmon \
            --replace \
            --detach \
            --userns=keep-id \
            --log-driver=journald \
            --security-opt label=disable \
            --pull ${pullPolicy} \
            --label=io.containers.autoupdate=registry \
            --device /dev/snd \
            --device /dev/gpiomem \
            --group-add 997 \
            --env TZ=${config.time.timeZone} \
            --env XDG_RUNTIME_DIR=${xdgRuntimeDir} \
            --env DBUS_SESSION_BUS_ADDRESS=unix:path=${xdgRuntimeDir}/bus \
            --env-file ${config.sops.secrets.hivemindSatEnv.path} \
            --volume ${xdgRuntimeDir}/bus:${xdgRuntimeDir}/bus:ro \
            --volume ${xdgRuntimeDir}/pipewire-0:${xdgRuntimeDir}/pipewire-0:ro \
            --volume /tmp/mycroft:/tmp/mycroft:z \
            --volume /home/ovos/cache/ovos_nltk:/home/ovos/nltk_data \
            --volume /home/ovos/cache/ovos_listener_records:/home/ovos/.local/share/mycroft/listener \
            --volume /home/ovos/cache/ovos_models:/home/ovos/.local/share/precise-lite \
            --volume /home/ovos/cache/ovos_vosk:/home/ovos/.local/share/vosk \
            --volume /home/ovos/config/hivemind:/home/ovos/.config/hivemind \
            --volume /home/ovos/config/mycroft:/home/ovos/.config/mycroft \
            --volume /home/ovos/share/hivemind:/home/ovos/.local/share/hivemind \
            --volume /home/ovos/share/mycroft:/home/ovos/.local/share/mycroft \
            ${image}:${tag}
        '';
        ExecStop = "${podman} stop --ignore -t 10 --cidfile=%t/%n.ctr-id";
        ExecStopPost = "${podman} rm -f --ignore -t 10 --cidfile=%t/%n.ctr-id";
        Type = "notify";
        NotifyAccess = "all";
      };
    };
  };
}
