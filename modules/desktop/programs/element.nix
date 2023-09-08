{
  options,
  config,
  lib,
  pkgs,
  edge,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.programs.element;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.element = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.packages = [edge.element-desktop];
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          {
            method = "symlink";
            directory = ".config/Element";
          }
          {
            method = "symlink";
            directory = ".config/Element-work";
          }
          {
            method = "symlink";
            directory = ".config/Element-personal";
          }
        ];
      };
      home.file.".local/share/applications/element-desktop-work.desktop" = {
        text = ''
          [Desktop Entry]
          Categories=Network;InstantMessaging;Chat
          Comment=Element Work
          Exec=element-desktop --profile=work --proxy-server=socks5://10.64.0.1:1080 %u
          GenericName=Matrix Work
          Icon=element
          MimeType=x-scheme-handler/element
          Name=Element Work
          StartupWMClass=element-work
          Type=Application
          Version=1.4
        '';
      };

      home.file.".local/share/applications/element-desktop-personal.desktop" = {
        text = ''
          [Desktop Entry]
          Categories=Network;InstantMessaging;Chat
          Comment=Element Personal
          Exec=element-desktop --profile=personal --proxy-server=socks5://127.0.0.1:1081 %u
          GenericName=Matrix Personal
          Icon=element
          MimeType=x-scheme-handler/element
          Name=Element Personal
          StartupWMClass=element-personal
          Type=Application
          Version=1.4
        '';
      };
    };
  };
}
