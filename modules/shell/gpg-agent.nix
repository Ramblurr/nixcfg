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
  cfg = config.modules.shell.gpg-agent;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.gpg-agent = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    services.pcscd.enable = true;
    myhm =
      { ... }@hm:
      {
        services.gpg-agent =
          {
            enable = true;
            enableSshSupport = true;
            enableZshIntegration = true;
            defaultCacheTtl = 60;
            maxCacheTtl = 120;
            enableExtraSocket = true;
            enableBashIntegration = true;
            sshKeys = [ "978C4D08058BA26EB97CB51820782DBCACFAACDA" ];
          }
          // (
            if builtins.hasAttr "pinentry" hm.options.services.gpg-agent then
              { pinentry.package = pkgs.pinentry-qt; }
            else
              { pinentryPackage = pkgs.pinentry-qt; }
          );
        programs.gpg = {
          enable = true;
          homedir = "${hm.config.xdg.configHome}/.gnupg";
          publicKeys = [
            {
              source = ../../configs/casey-pub.asc;
              trust = "ultimate";
            }
          ];
          scdaemonSettings = {
            disable-ccid = true;
            card-timeout = "2";
            pcsc-shared = true;
            debug-level = "basic";
            log-file = "${hm.config.home.homeDirectory}/.cache/scdaemon.log";
          };
        };

        persistence = mkIf withImpermanence { directories = [ "${hm.config.xdg.configHome}/.gnupg" ]; };
      };
  };
}
