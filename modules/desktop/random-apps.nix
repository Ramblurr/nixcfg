{
  options,
  config,
  lib,
  pkgs,
  inputs,
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
    myhm =
      { ... }@hm:
      {
        home.packages = lib.mkMerge [
          #(lib.mkIf (pkgs.hostPlatform.system == "x86_64-linux") (with pkgs; [
          #  # x86_64-linux only
          #  zenith # system monitor
          #]))
          (lib.mkIf (pkgs.hostPlatform.system == "aarch_64-linux") (
            with pkgs;
            [
              # aarch64-linux only
            ]
          ))
          (with pkgs; [
            appimage-run
            audacity
            pavucontrol
            # upstream is broken https://github.com/NixOS/nixpkgs/issues/341043
            #openscad-unstable
            openscad-lsp
            #freecad
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
            morgen
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
            xournal
            fava
            beancount
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
