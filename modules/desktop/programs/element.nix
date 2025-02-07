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
  cfg = config.modules.desktop.programs.element;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.element = {
    enable = lib.mkEnableOption "";
    workProxy = lib.mkOption {
      type = types.str;
      description = "The proxy server to use for the work profile";
    };
  };
  config = mkIf cfg.enable {

    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d '/persist${homeDirectory}/.config/iamb' - ${username} ${username} - -"
      "d '/persist${homeDirectory}/.config/Element' - ${username} ${username} - -"
      "d '/persist${homeDirectory}/.config/Element-work' - ${username} ${username} - -"
      "d '/persist${homeDirectory}/.config/Element-personal' - ${username} ${username} - -"
    ];

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Element"
          ".config/Element-work"
          ".config/Element-personal"
          ".config/iamb"
        ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = [
          pkgs.element-desktop
          #(pkgs.element-desktop.override { electron = pkgs.electron_30; })
          pkgs.iamb
        ];

        #home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        #  directories = [
        #    {
        #      method = "symlink";
        #      directory = ".config/Element";
        #    }
        #    {
        #      method = "symlink";
        #      directory = ".config/Element-work";
        #    }
        #    {
        #      method = "symlink";
        #      directory = ".config/Element-personal";
        #    }
        #    ".config/iamb"
        #  ];
        #};
        home.file.".local/share/applications/element-desktop-work.desktop" = {
          text = ''
            [Desktop Entry]
            Categories=Network;InstantMessaging;Chat
            Comment=Element Work
            Exec=element-desktop --use-gl=desktop --profile=work --proxy-server=socks5://${cfg.workProxy} %u
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
            Exec=element-desktop --profile=personal --use-gl=desktop %u
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
