{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain lanVpnGateway;
  inherit (config.modules.users.primaryUser) username;
in
{
  imports = [
    inputs.microvm.nixosModules.host
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    inputs.nix-gaming.nixosModules.pipewireLowLatency
    ../../config
    ../../config/workstation-impermanence.nix
    ./hardware-configuration.nix
    ./networking.nix
    ./wireplumber.nix
    ./syncthing.nix
    ./roon-bridge.nix
    ./libvirt.nix
    ./gaming.nix
    ../../modules/site-net
    #./microvm-host-opts.nix
    #./microvm-host.nix
    #./arm.nix
  ];
  system.stateVersion = "23.05";
  sops.defaultSopsFile = ./secrets.sops.yaml;
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  networking.hostId = lib.my.generateHostId config.networking.hostName;

  time.timeZone = "Europe/Berlin";

  sops.secrets.HASS_TOKEN = {
    owner = username;
    mode = "0400";
  };
  sops.secrets.netrc = {
    owner = username;
    mode = "0400";
  };
  sops.templates.hacompanion-env = {
    owner = username;
    mode = "0400";
    content = ''
      HASS_TOKEN=${config.sops.placeholder."HASS_TOKEN"}
      HASS_DEVICE_NAME=quine
    '';
  };
  sops.secrets."github_token" = {
    owner = username;
    mode = "0400";
  };

  sops.templates."nix.conf".owner = username;
  sops.templates."nix.conf".content = ''
    access-tokens = github.com=${config.sops.placeholder."github_token"}
  '';

  nix = {
    settings = {
      max-jobs = 16;
      cores = 16;
      netrc-file = config.sops.secrets.netrc.path;
    };

    extraOptions = ''
      !include ${config.sops.templates."nix.conf".path}
    '';
  };

  home.nix-lan-cache.enable = true;

  networking.firewall.allowedTCPPorts = [
    3000
    3001
    8080
    8081
    8090
    1080
  ];
  networking.firewall.allowedUDPPorts = [ 67 ];

  modules = {
    desktop = {
      wayland.enable = true;
      #hyprland.enable = true;
      niri.enable = true;
      hyprpaper.enable = true;
      mako.enable = true;
      dconf.enable = true;
      greetd.enable = true;
      xdg.enable = true;
      random-apps.enable = true;
      fonts = {
        enable = true;
        packages = [ pkgs.iosevka-bin ];
      };
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
      };
      gaming.enable = false;
      services = {
        ha-shutdown.enable = false;
        hacompanion = {
          enable = false;
          environmentFile = config.sops.templates.hacompanion-env.path;
          unitAfter = [ "sops-nix.service" ];
          listenPort = 6669;
          settings = {
            homeassistant = {
              device_name = "quine";
              host = "https://home.${domain.home}";
            };
            notifications = {
              push_url = "http://10.9.4.3:6669/notifications";
              listen = ":6669";
            };
          };
        };
        swhkd = {
          enable = true;
          keybindings = lib.mapAttrs (_: v: lib.mkDefault v) {
            "super + e" = "wezterm";
          };
        };
      };
      programs = {
        #aseprite.enable = true;
        #calibre.enable = true;
        cad.enable = true;
        discord.enable = false;
        #element.enable = true;
        #element.work.enable = true;
        #element.workProxy = "${lanVpnGateway}:1081";
        fritzing.enable = false;
        junction.enable = true;
        kdeconnect.enable = true;
        kicad.enable = false;
        kitty.enable = true;
        wezterm.enable = true;
        ghostty.enable = true;
        logseq.enable = true;
        musescore.enable = true;
        nextcloud.enable = true;
        #nheko.enable = true;
        obs.enable = true;
        onepassword.enable = true;
        onepassword.autostart.enable = true;
        owncloud.enable = true;
        signal.enable = true;
        slack.enable = false;
        thunderbird.enable = true;
        thunderbird.autostart.enable = true;
        thunderbird.work.enable = true;
        thunderbird.workProxy = "${lanVpnGateway}:1081";
        handy.enable = true;
        handy.autostart.enable = true;
        waydroid.enable = true;
        yubico.enable = true;
        yubico.sshFidoAgent.enable = true;
      };
    };
    shell = {
      aria2.enable = true;
      attic.enable = true;
      atuin.enable = true;
      atuin.syncing.enable = true;
      direnv.enable = true;
      ffsend.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      htop.enable = true;
      isd.enable = true;
      mpv.enable = true;
      ssh.enable = true;
      tmux.enable = true;
      zellij.enable = true;
      #zellij.web.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
      zsh.powerlevel10k.enable = true;
      random.enable = true;
    };
    services = {
      docker.enable = true;
      attic-watch-store.enable = true;
      docker.enableOnBoot = false;
      podman.enable = true;
      nomad.enable = false;
      microsocks.enable = false;
      printing.enable = true;
      printing.drivers = [ pkgs.cups-brother-mfcl2750dw ];
      sshd.enable = true;
      flatpak.enable = true;
    };
    dev = {
      clojure.enable = true;
      janet.enable = true;
      #fennel.enable = true;
      jetbrains.enable = true;
      node.enable = true;
      python.enable = true;
      random.enable = true;
      llms.enable = true;
      llms.cudaSupport = true;
    };

    editors = {
      emacs.enable = true;
      emacs.package = pkgs.emacs-pgtk;
      vim = {
        enable = true;
        extraPlugins = with pkgs.vimPlugins; [
          vim-surround
          vim-airline
          tabular
          vim-nix
          gruvbox-community
        ];
      };
    };
    impermanence.enable = true;
    boot.zfs.enable = true;
    boot.zfs.scrubPools = [ "rpool" ];
    vpn.mullvad.enable = false;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    hardware.keyboardio.enable = true;
    hardware.om-footswitch.enable = true;
    hardware.ryzen.enable = true;
    hardware.easyNvidia = {
      enable = true;
      withIntegratedGPU = false;
      advanced = {
        monitorControlSupport = true;
        usePageAttributeTable = true;
      };
      desktopEnvironment = "wlroots";
      vaapi = {
        enable = true;
        firefox = {
          enable = true;
          av1Support = true;
        };
      };
    };
    hardware.pipewire.enable = true;
    hardware.pipewire.denoise.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
      "dialout"
      "kvm"
      "docker"
      "audio"
      "flatpak"
      "input"
      "plugdev"
      "libvirtd"
      "media"
      "cdrom"
    ];
    #services.pcscd.readerConfig = '''';
    services.borgmatic = {
      enable = true;
      name = "aquinas.${domain.home}-mali";
      repositories = [
        {
          label = "mali";
          path = "\${NAS_REPOSITORY}";
        }
        {
          label = "offsite2";
          path = "\${OFFSITE_REPOSITORY2}";
        }
      ];
      exclude-patterns = [
        "etc/ssl"
        "persist/home/*/.m2"
        "persist/home/*/.cache"
        "persist/home/*/.var"
        "persist/home/*/.local/lib"
        "persist/home/*/.local/share/containers"
        "persist/home/*/.local/share/JetBrains"
        "persist/home/*/.local/share/volta"
        "persist/home/*/.local/share/lein"
        "persist/home/*/.local/share/Trash"
        "persist/home/*/.local/share/virtualenv"
        "persist/home/*/.local/share/yarn"
        "persist/home/*/.local/share/nvm"
        "persist/home/*/.local/state"
        "persist/home/*/.npm"
        "persist/home/*/.yarn"
        "persist/home/*/.vagrant.d/boxes"
        "'*.pyc'"
        "'*/.vim*.tmp'"
        "'*/.DS_Store'"
        "'*/node_modules'"
        "'*/build'"
        "'*/target'"
        "'*/dist'"
        "'*/tmp'"
        "'*/bower_components'"
        "'*.idea'"
        "'*/.*~'"
        "'*/out'"
        "'*/.vagrant'"
        "'*/securedir'"
        "'*/encrypted'"
        "'*/ram'"
        "'*/cache'"
        "'*/.cache'"
        "'*/_cacache'"
        "'*/_lock'"
        "'*/*.tmp'"
        "'*/*.swp'"
        "'*/*~'"
        "'*/*.lock'"
        "'*/*-nas'"
        "'*/.Trash'"
        "'*/.terraform'"
        "'*/venv'"
        #"'*/emacs-doom.d'"
        "'*/.gradle'"
        "'*/.*sync*.db'"
        "'*/.ansible'"
      ];
    };
  };
}
