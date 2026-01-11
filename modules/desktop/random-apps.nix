{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.random-apps;
in
{
  options.modules.desktop.random-apps = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    myhm = _: {
      home.packages = lib.mkMerge [
        #(lib.mkIf (pkgs.stdenv.hostPlatform.system == "x86_64-linux") (with pkgs; [
        #  # x86_64-linux only
        #  zenith # system monitor
        #]))
        (lib.mkIf (pkgs.stdenv.hostPlatform.system == "aarch_64-linux") (
          with pkgs;
          [
            # aarch64-linux only
          ]
        ))
        (with pkgs; [
          vlc
          speedcrunch
          xournalpp
          libreoffice
          appimage-run
          audacity
          pavucontrol
          prusa-slicer
          #eagle
          #xcircuit
          brightnessctl
          virt-viewer
          evince
          kooha
          #onlyoffice-bin
          #libreoffice-qt
          #hunspell
          #hunspellDicts.en_US
          #hunspellDicts.de_AT
          ffmpeg-full
          libnotify # `notify-send`
          gparted
          inkscape
          #krita
          meld
          #gimp-with-plugins
          gimp
          pdfgrep
          nerdfix
          kid3
          pdfarranger
          #makemkv
          # BLOCKED: https://github.com/NixOS/nixpkgs/issues/380197
          keepassxc
          dex
          qpwgraph # pipewire wiring gui tool
          easyeffects # pipewire eq
        ])
      ];

      persistence = {
        files = [
          ".config/kritarc"
          ".config/kritadisplayrc"
        ];
      };
    };
  };
}
