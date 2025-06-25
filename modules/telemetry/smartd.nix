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
    services.smartd = {
      enable = true;
      notifications = {
        test = true;
      };
    };
  };
}
