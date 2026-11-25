{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.smartd;
in
{
  options.modules.telemetry.smartd = {
    enable = lib.mkEnableOption "smartd";
  };

  config = lib.mkIf cfg.enable {
    modules.motd.units = [ config.systemd.services.smartd.name ];
    services.smartd = {
      enable = true;
      notifications = {
        test = false;
      };
    };
  };
}
