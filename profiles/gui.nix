{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  # _chrome = pkgs.ungoogled-chromium;
  _chrome = pkgs.google-chrome-dev.override {
    commandLineArgs = ["--force-dark-mode"];
  };
in {
  imports = [
    ./interactive.nix # includes core.nix (which imports hm)

    ../mixins/kitty.nix
    ../mixins/fonts.nix
    ../mixins/gtk.nix
    ../mixins/pipewire.nix
    ../mixins/kdeconnect.nix
    ../mixins/flatpak.nix
    ../mixins/syncthing.nix
    ../mixins/1password.nix
    ../mixins/emacs.nix
    ../mixins/firefox.nix
    ../mixins/thunderbird.nix
    ../mixins/mpv.nix
    ../mixins/vpn-mullvad.nix
  ];

  config = {
    # hardware.drivers.enable = true;
    hardware.bluetooth = {
      enable = false;
      powerOnBoot = false;
      settings = {
        General = {
          Experimental = true;
        };
      };
    };

    programs.noisetorch.enable = true;

    services = {};

    home-manager.users.ramblurr = {
      pkgs,
      config,
      ...
    } @ hm: {
      # home-manager/#2064
      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = ["graphical-session-pre.target"];
        };
      };

      home.sessionVariables = {
        BROWSER = "firefox";
      };

      services = {
      };

      home.packages = lib.mkMerge [
        (lib.mkIf (pkgs.hostPlatform.system == "x86_64-linux") (with pkgs; [
          _chrome
          captive-browser
        ]))
        (with pkgs; [
          # misc tools/utils
          pavucontrol
          brightnessctl
          virt-viewer
          evince
          #signal-desktop
          #vlc
          #onlyoffice-bin
          #libreoffice-qt
          #hunspell
          #hunspellDicts.en_US
          #hunspellDicts.de_AT
          morgen
          junction
          ffmpeg_5-full
          libnotify # `notify-send`
        ])
      ];
    };
  };
}
