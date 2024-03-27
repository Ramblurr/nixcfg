{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.firefox;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.browsers.firefox = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = { pkgs, ... }@hm: {
      programs.firefox = {
        enable = true;
        package =
          inputs.firefox-nightly.packages.${pkgs.stdenv.hostPlatform.system}.firefox-nightly-bin;
        profiles.personal = {
          id = 0;
          path = "personal";
        };

        profiles.work = {
          id = 1;
          path = "work";
        };

        profiles.dev = {
          id = 2;
          path = "dev";
          settings = {
            # always do a clean start
            "browser.sessionstore.resume_from_crash" = false;
          };
        };
      };

      home.persistence."/persist${homeDirectory}" = {
        directories = [ ".mozilla/extensions" ".mozilla/firefox" ".cache/mozilla/firefox" ];
      };

      home.file.".local/share/applications/firefox-work.desktop" = {
        text = ''
          [Desktop Entry]
          Actions=new-private-window
          Categories=Network;WebBrowser
          Exec=firefox-nightly -P work --name firefox-work --class=firefox-work %U
          GenericName=Work Web Browser
          Icon=firefox
          MimeType=text/html;text/xml;application/xhtml+xml;application/vnd.mozilla.xul+xml;x-scheme-handler/http;x-scheme-handler/https
          Name=Work Firefox
          StartupNotify=true
          StartupWMClass=firefox-work
          Terminal=false
          Type=Application
          Version=1.4
          [Desktop Action new-private-window]
          Exec=firefox-nightly -P work --name firefox-work --class=firefox-work --private-window %U
          Name=New Work Private Window
        '';
      };
      home.file.".local/share/applications/firefox-personal.desktop" = {
        text = ''
          [Desktop Entry]
          Actions=new-private-window
          Categories=Network;WebBrowser
          Exec=firefox-nightly -P personal --name firefox-personal --class=firefox-personal %U
          GenericName=Personal Web Browser
          Icon=firefox
          MimeType=text/html;text/xml;application/xhtml+xml;application/vnd.mozilla.xul+xml;x-scheme-handler/http;x-scheme-handler/https
          Name=Personal Firefox
          StartupNotify=true
          StartupWMClass=firefox-personal
          Terminal=false
          Type=Application
          Version=1.4
          [Desktop Action new-private-window]
          Exec=firefox-nightly -P personal --name firefox-personal --class=firefox-personal --private-window %U
          Name=New Personal Private Window
        '';
      };
    };
  };
}
