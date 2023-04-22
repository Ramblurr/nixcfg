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
          firefox
          _chrome
          captive-browser
        ]))
        (with pkgs; [
          # misc tools/utils
          pavucontrol
          brightnessctl
          virt-viewer
          evince
          signal-desktop
          # libnotify # `notify-send`
        ])
      ];
    };
  };
}
