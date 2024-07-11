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
  cfg = config.modules.desktop.xdg;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.xdg = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    xdg.portal = {
      enable = true;
      xdgOpenUsePortal = true;
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        files = [ ".config/mimeapps.list" ];
      };
    };

    myhm =
      { pkgs, ... }@hm:
      {
        home.packages = with pkgs; [
          xdg-utils
          xdg-user-dirs
        ];
        xdg = {
          enable = true;
          userDirs = {
            enable = true;
            desktop = "$HOME/desktop";
            documents = "$HOME/docs";
            download = "$HOME/downloads";
            music = "$HOME/tmp/music";
            pictures = "$HOME/docs/img";
            publicShare = "$HOME/docs/public";
            templates = "$HOME/docs/templates";
            videos = "$HOME/tmp/videos";
            extraConfig = {
              XDG_SCREENSHOTS_DIR = "${hm.config.xdg.userDirs.pictures}/screenshots";
            };
          };
          #mimeApps = {
          #  enable = true;
          #  defaultApplications =
          #    let
          #      browser = [ "re.sonny.Junction.desktop" ];
          #    in
          #    {
          #      "x-scheme-handler/file" = browser;
          #      "inode/directory" = browser;
          #      "text/html" = browser;
          #      "x-scheme-handler/http" = browser;
          #      "x-scheme-handler/https" = browser;
          #      "x-scheme-handler/about" = browser;
          #      "x-scheme-handler/unknown" = browser;
          #      "x-scheme-handler/vscode" = browser;
          #      "x-scheme-handler/discord" = [ "discord.desktop" ];
          #      "audio/*" = browser;
          #      "video/*" = browser;
          #      "image/*" = browser;
          #      "image/gif" = browser;
          #      "image/jpeg" = browser;
          #      "image/png" = browser;
          #      "image/webp" = browser;
          #    };
          #  associations.added = {
          #    "image/jpeg" = [ "org.kde.gwenview.desktop" ];
          #    "inode/directory" = [ "org.kde.dolphin.desktop" ];
          #    "image/svg+xml" = [ "org.inkscape.Inkscape.desktop" ];
          #    "x-scheme-handler/https" = [
          #      "firefox-personal.desktop"
          #      "firefox-work.desktop;firefox.desktop"
          #    ];
          #    "image/png" = [ "org.kde.gwenview.desktop" ];
          #    "x-scheme-handler/vscode" = [ "code-work-url-handler.desktop" ];
          #    "application/pdf" = [ "okularApplication_pdf.desktop" ];
          #    "text/html" = [
          #      "firefox-personal.desktop"
          #      "firefox-work.desktop;firefox.desktop"
          #    ];
          #    "x-scheme-handler/http" = [ "firefox.desktop" ];
          #  };
          #};
        };
      };
  };
}
