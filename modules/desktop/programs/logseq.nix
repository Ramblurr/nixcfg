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
  cfg = config.modules.desktop.programs.logseq;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.logseq = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Logseq"
          ".logseq"
        ];
      };
    };

    nixpkgs.config.permittedInsecurePackages = [ "electron-27.3.11" ];

    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = [ pkgs.logseq ];
        home.file.".local/share/applications/logseq.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Logseq
            Exec=env -u NIXOS_OZONE_WL logseq %u
            Terminal=false
            Type=Application
            Icon=logseq
            StartupWMClass=Logseq
            X-AppImage-Version=0.10.7
            Comment=A privacy-first, open-source platform for knowledge management and collaboration.
            MimeType=x-scheme-handler/logseq
            Categories=Utility
          '';
        };
      };
  };
}
