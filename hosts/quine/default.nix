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
    ./microvm-test.nix
    ./microvm-test-impl.nix
  ];
  system.stateVersion = "23.05";
  sops.defaultSopsFile = ./secrets.sops.yaml;
  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;

  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";

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
      substituters = [
        "https://hyprland.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
      netrc-file = config.sops.secrets.netrc.path;
    };

    extraOptions = ''
      !include ${config.sops.templates."nix.conf".path}
    '';
  };
  home.attic.enable = true;

  networking.firewall.allowedTCPPorts = [
    3000
    3001
    8080
    8081
  ];
  networking.firewall.allowedUDPPorts = [ 67 ];

  modules = {
    desktop = {
      hyprland3.enable = true;
      xdg.enable = true;
      random-apps.enable = true;
      fonts =
        let
          # Pragmata Pro Style
          iosevka-ss08 = (pkgs.iosevka-bin.override { variant = "SS08"; });
          # Source Code Pro Style
          iosevka-ss09 = (pkgs.iosevka-bin.override { variant = "SS09"; });
          # IBM Plex Mono Style
          iosevka-ss15 = (pkgs.iosevka-bin.override { variant = "SS15"; });
        in
        {
          enable = true;
          mono.name = "Iosevka SS15";
          mono.package = iosevka-ss15;
          terminal.name = "Iosevka Term SS15";
          terminal.package = iosevka-ss15;
          symbols.fonts = [
            "Iosevka Term SS15"
          ];
          packages = with pkgs; [
            (iosevka-bin.override { variant = "Aile"; })
            (iosevka-bin.override { variant = "Etoile"; })
            iosevka-ss08
            iosevka-ss09
            iosevka-ss15
            iosevka-bin
          ];
        };
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
      };
      gaming.enable = false;
      services = {
        ha-shutdown = {
          enable = true;
        };
        hacompanion = {
          enable = true;
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
        chrysalis.enable = true;
        discord.enable = false;
        element.enable = true;
        element.work.enable = true;
        element.workProxy = "${lanVpnGateway}:1081";
        fritzing.enable = false;
        junction.enable = true;
        kdeconnect.enable = true;
        kicad.enable = false;
        kitty.enable = true;
        wezterm.enable = true;
        logseq.enable = true;
        musescore.enable = true;
        #nextcloud.enable = true;
        #nheko.enable = true;
        obs.enable = false;
        onepassword.enable = true;
        owncloud.enable = true;
        signal.enable = true;
        slack.enable = false;
        thunderbird.enable = true;
        thunderbird.work.enable = true;
        thunderbird.workProxy = "${lanVpnGateway}:1081";
        yubico.enable = true;
      };
    };
    shell = {
      aria2.enable = true;
      attic.enable = true;
      atuin.enable = true;
      atuin.sync.enable = true;
      direnv.enable = true;
      ffsend.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      htop.enable = true;
      mpv.enable = true;
      ssh.enable = true;
      tmux.enable = true;
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
      microsocks.enable = false;
      printing.enable = true;
      printing.drivers = [ pkgs.cups-brother-mfcl2750dw ];
      sshd.enable = true;
      flatpak.enable = true;
    };
    dev = {
      clojure.enable = true;
      #fennel.enable = true;
      jetbrains.enable = true;
      k8s.enable = true;
      node.enable = true;
      python.enable = true;
      random.enable = true;
      #radicle.enable = true;
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
      vscode.enable = true;
    };
    impermanence.enable = true;
    boot.zfs.enable = true;
    boot.zfs.scrubPools = [ "rpool" ];
    vpn.mullvad.enable = false;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    hardware.ryzen.enable = true;
    hardware.easyNvidia = {
      enable = true;
      withIntegratedGPU = false;
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
      "kvm"
      "docker"
      "audio"
      "flatpak"
      "input"
      "plugdev"
      "libvirtd"
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
