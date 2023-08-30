{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.random-apps;
in {
  options.modules.desktop.random-apps = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    myhm = {...} @ hm: {
      home.packages = lib.mkMerge [
        (lib.mkIf (pkgs.hostPlatform.system == "x86_64-linux") (with pkgs; [
          # x86_64-linux only
          zenith # system monitor
        ]))
        (lib.mkIf (pkgs.hostPlatform.system == "aarch_64-linux") (with pkgs; [
          # aarch64-linux only
        ]))
        (with pkgs; [
          pavucontrol
          brightnessctl
          virt-viewer
          evince
          kooha
          #onlyoffice-bin
          #libreoffice-qt
          #hunspell
          #hunspellDicts.en_US
          #hunspellDicts.de_AT
          morgen
          ffmpeg_5-full
          libnotify # `notify-send`
          gparted
          inkscape
          meld
          gimp-with-plugins
          pdfgrep
          nerdfix
          kid3
          pdfarranger
          handbrake
          makemkv
          xournal
          fava
          beancount
          keepassxc
          dex
        ])
      ];

      persistence = {
        directories = [
          "docs"
          "downloads"
          "sync"
          "src"
          "work"
          "vendor"
          "nixcfg"
          ".local/bin"
          ".config/gnupg"
          ".config/gh"
          ".config/Morgen"
          ".config/OpenSCAD"
          ".config/PrusaSlicer-alpha"
          ".config/qobuz-dl"
          ".local/share/fonts"
          ".cache/inkscape"
          ".config/inkscape"
          ".config/GIMP"
          ".cache/gimp"
          ".config/rclone"
        ];
      };
    };
  };
}