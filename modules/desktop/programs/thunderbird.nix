{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.desktop.programs.thunderbird;

  proxyParts = lib.splitString ":" cfg.work.proxy;
  proxyAddr = builtins.elemAt proxyParts 0;
  proxyPort = lib.toInt (builtins.elemAt proxyParts 1);
in
{
  options.modules.desktop.programs.thunderbird = {
    enable = lib.mkEnableOption "";
    autostart.enable = lib.mkEnableOption "";
    work = {
      enable = lib.mkEnableOption "the work Thunderbird profile";
      proxy = lib.mkOption {
        type = lib.types.str;
        description = "SOCKS proxy for the work profile, as host:port";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    myhm = hm: {
      programs.thunderbird = {
        enable = true;
        profiles = {
          personal = {
            isDefault = true;
            withExternalGnupg = true;
            extensions = [
              pkgs.betterunsubscribe
            ]
            ++ lib.optional config.modules.desktop.programs.thunderbird-cli.enable pkgs.thunderbird-ai-bridge;
            settings."extensions.autoDisableScopes" = 0;
          };
          work = lib.mkIf cfg.work.enable {
            withExternalGnupg = true;
            settings = {
              "network.proxy.socks" = proxyAddr;
              "network.proxy.socks_port" = proxyPort;
              "network.proxy.socks_remote_dns" = true;
              "network.proxy.type" = 1;
            };
          };
        };
      };

      home.file.".config/autostart/thunderbird-personal.desktop" = lib.mkIf cfg.autostart.enable (
        lib.my.autostart "thunderbird --name thunderbird-personal --profile ${hm.config.home.homeDirectory}/.thunderbird/personal"
      );
      home.file.".local/share/applications/thunderbird-personal.desktop" = {
        text = ''
          [Desktop Entry]
          Categories=Network;Chat;Email;Feed;GTK;News
          Comment=Read and write e-mails or RSS feeds, or manage tasks on calendars.
          Exec=thunderbird --name thunderbird-personal --profile ${hm.config.home.homeDirectory}/.thunderbird/personal %U
          GenericName=Personal Email Client
          Icon=thunderbird
          Keywords=mail;email;e-mail;messages;rss;calendar;address book;addressbook;chat
          MimeType=message/rfc822;x-scheme-handler/mailto;text/calendar;text/x-vcard
          Name=Personal Thunderbird
          StartupNotify=true
          StartupWMClass=thunderbird-personal
          Terminal=false
          Type=Application
          Version=1.4
        '';
      };

      home.file.".local/share/applications/thunderbird-work.desktop" = lib.mkIf cfg.work.enable {
        text = ''
          [Desktop Entry]
          Categories=Network;Chat;Email;Feed;GTK;News
          Comment=Read and write e-mails or RSS feeds, or manage tasks on calendars.
          Exec=thunderbird --name thunderbird-work --profile ${hm.config.home.homeDirectory}/.thunderbird/work %U
          GenericName=Work Email Client
          Icon=thunderbird
          Keywords=mail;email;e-mail;messages;rss;calendar;address book;addressbook;chat
          MimeType=message/rfc822;x-scheme-handler/mailto;text/calendar;text/x-vcard
          Name=Work Thunderbird
          StartupNotify=true
          StartupWMClass=thunderbird-work
          Terminal=false
          Type=Application
          Version=1.4
        '';
      };
    };
  };
}
