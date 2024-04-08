{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.thunderbird;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.thunderbird = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    myhm =
      { ... }@hm:
      {
        programs.thunderbird = {
          enable = true;
          profiles = {
            personal = {
              isDefault = true;
              withExternalGnupg = true;
            };
            work = {
              withExternalGnupg = true;
              settings = {
                "network.proxy.socks" = "10.64.0.1";
                "network.proxy.socks_port" = 1080;
                "network.proxy.socks_remote_dns" = true;
                "network.proxy.type" = 1;
              };
            };
          };
        };

        persistence = {
          directories = [ ".thunderbird" ];
        };

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

        home.file.".local/share/applications/thunderbird-work.desktop" = {
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
