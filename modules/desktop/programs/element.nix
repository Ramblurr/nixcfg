{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.desktop.programs.element;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.element = {
    enable = lib.mkEnableOption "";
    work.enable = lib.mkEnableOption "";
    workProxy = lib.mkOption {
      type = lib.types.str;
      description = "The proxy server to use for the work profile";
    };
  };
  config = lib.mkIf cfg.enable {

    systemd.tmpfiles = lib.mkIf withImpermanence {
      rules = [
        "d '/persist${homeDirectory}/.config/iamb' - ${username} ${username} - -"
        "d '/persist${homeDirectory}/.config/Element' - ${username} ${username} - -"
        "d '/persist${homeDirectory}/.config/Element-personal' - ${username} ${username} - -"
      ]
      ++ (lib.optionals cfg.work.enable [
        "d '/persist${homeDirectory}/.config/Element-work' - ${username} ${username} - -"
      ]);
    };

    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Element"
          ".config/Element-personal"
          ".config/iamb"
        ]
        ++ lib.optionals cfg.work.enable [ ".config/Element-work" ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [
          pkgs.element-desktop
          pkgs.iamb
        ];

        home.file.".local/share/applications/element-desktop-work.desktop" = lib.mkIf cfg.work.enable {
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
