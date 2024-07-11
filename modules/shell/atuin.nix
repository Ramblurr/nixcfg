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
  cfg = config.modules.shell.atuin;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.atuin = {
    enable = mkBoolOpt false;
    sync.enable = mkBoolOpt false;
    sync.address = mkStrOpt "https://atuin.socozy.casa";
  };
  config = lib.mkIf cfg.enable {

    myhm =
      { pkgs, config, ... }@hm:
      {
        programs.atuin = {
          enable = true;
          enableBashIntegration = true;
          settings =
            {
              style = "compact";
              update_check = false;
            }
            // lib.optionalAttrs cfg.sync.enable {
              sync_address = cfg.sync.address;
              auto_sync = false;
            };
        };
        home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
          directories = [ ".config/atuin" ];
        };
        home.file = mkIf withImpermanence {
          ".local/share/atuin".source = config.lib.file.mkOutOfStoreSymlink "/persist/extra/atuin";
        };

        systemd.user.timers.atuin-sync = lib.mkIf cfg.sync.enable {
          Unit.Description = "Atuin auto sync";
          Timer.OnUnitActiveSec = "1h";
          Install.WantedBy = [ "timers.target" ];
        };

        systemd.user.services.atuin-sync = lib.mkIf cfg.sync.enable {
          Unit.Description = "Atuin auto sync";

          Service = {
            Type = "oneshot";
            ExecStart = "${pkgs.atuin}/bin/atuin sync";
            IOSchedulingClass = "idle";
          };
        };
      };
  };
}
