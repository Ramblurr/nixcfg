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
  cfg = config.modules.desktop.browsers.firefox;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  firefox-nightly =
    inputs.firefox-nightly.packages.${pkgs.stdenv.hostPlatform.system}.firefox-nightly-bin;
  firefox-devedition-bin =
    inputs.firefox-nightly.packages.${pkgs.stdenv.hostPlatform.system}.firefox-devedition-bin;
  firefox = pkgs.firefox-bin;
in
{
  options.modules.desktop.browsers.firefox = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.systemPackages = [ pkgs.firefoxpwa ];

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".mozilla/extensions"
          ".mozilla/firefox"
          ".cache/mozilla/firefox"
        ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      {
        #home.packages = [ firefox-devedition-bin ];
        programs.firefox = {
          enable = true;
          package = firefox;
          nativeMessagingHosts = [ pkgs.firefoxpwa ];
          profiles.personal = {
            id = 0;
            path = "personal";
            settings = {
              "browser.tabs.loadInBackground" = true;
            };
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
      };
  };
}
