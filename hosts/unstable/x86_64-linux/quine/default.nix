{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "quine";
  defaultSopsFile = ./secrets.sops.yaml;
in {
  imports = [
    ../../../home.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    ./hardware-configuration.nix
    ./networking.nix
    ./wireplumber.nix
    ./syncthing.nix
    ./roon-bridge.nix
  ];
  system.stateVersion = "23.05";
  sops.defaultSopsFile = defaultSopsFile;
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "76913090587c40c8a3207202dfe86fc2";
  services.udev.extraRules = ''
    KERNEL=="ttyACM0", MODE:="666"
  '';

  sops.secrets.HASS_TOKEN = {
    owner = "ramblurr";
    mode = "0400";
  };
  sops.templates.hacompanion-env = {
    owner = "ramblurr";
    mode = "0400";
    content = ''
      HASS_TOKEN=${config.sops.placeholder."HASS_TOKEN"}
      HASS_DEVICE_NAME=quine
    '';
  };
  modules = {
    desktop = {
      enable = true;
      setupCommands = ''
        xrandr \
          --output DP-2 --mode 5120x1440 --auto
        xrandr \
        --output HDMI-A-1 --mode 3840x2160 --auto
      '';
      fonts.enable = true;
      hyprland.enable = true;
      kde.enable = true;
      xdg.enable = true;
      random-apps.enable = true;
      browsers = {
        firefox.enable = true;
        chromium.enable = true;
      };
      gaming.enable = true;
      services = {
        ha-shutdown = {
          enable = true;
        };
        hacompanion = {
          enable = true;
          environmentFile = config.sops.templates.hacompanion-env.path;
          unitAfter = ["sops-nix.service"];
          listenPort = 6669;
          settings = {
            homeassistant = {
              device_name = "quine";
              host = "https://home.***REMOVED***";
            };
            notifications = {
              push_url = "http://10.9.4.3:6669/notifications";
              listen = ":6669";
            };
          };
        };
      };
      programs = {
        aseprite.enable = true;
        calibre.enable = true;
        chrysalis.enable = true;
        discord.enable = true;
        element.enable = true;
        junction.enable = true;
        kdeconnect.enable = true;
        kitty.enable = true;
        logseq.enable = true;
        nextcloud.enable = true;
        nheko.enable = true;
        obs.enable = true;
        onepassword.enable = true;
        signal.enable = true;
        kicad.enable = true;
        slack.enable = true;
        thunderbird.enable = true;
        wezterm.enable = true;
      };
    };
    shell = {
      aria2.enable = true;
      atuin.enable = true;
      direnv.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      htop.enable = true;
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
      docker.enableOnBoot = false;
      podman.enable = true;
      microsocks.enable = true;
      printing.enable = true;
      printing.drivers = [pkgs.cups-brother-mfcl2750dw];
      sshd.enable = true;
      flatpak.enable = true;
    };
    dev = {
      clojure.enable = true;
      fennel.enable = true;
      jetbrains.enable = true;
      k8s.enable = true;
      node.enable = true;
      python.enable = true;
      random.enable = true;
    };

    editors = {
      emacs.enable = true;
      vim = {
        enable = true;
        extraPlugins = with pkgs.vimPlugins; [
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
    boot.zfs.scrubPools = ["rpool"];
    vpn.mullvad.enable = true;
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    networking.default.enable = true;
    networking.default.hostName = hn;
    hardware.ryzen.enable = true;
    hardware.nvidia.enable = true;
    hardware.pipewire.enable = true;
    hardware.pipewire.denoise.enable = true;
    users.enable = true;
    users.primaryUser = {
      username = "ramblurr";
      name = "Casey Link";
      homeDirectory = "/home/ramblurr";
      signingKey = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
      email = "unnamedrambler@gmail.com";
      passwordSecretKey = "ramblurr-password";
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
        "audio"
        "flatpak"
        "input"
        "plugdev"
      ];
    };
    services.borgmatic = {
      enable = true;
      name = "aquinas.***REMOVED***-mali";
      repositories = [
        {
          label = "mali";
          path = "ssh://borg@mali.int.***REMOVED***/mnt/tank2/backups/borg_repos/aquinas";
        }
        {
          label = "offsite";
          path = "\${OFFSITE_REPOSITORY}";
        }
      ];
      exclude-patterns = [
        "etc/ssl"
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
        "\'*.pyc\'"
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
        "'*/emacs-doom.d'"
        "'*/.gradle'"
        "'*/.*sync*.db'"
        "'*/.ansible'"
      ];
    };
  };
}
