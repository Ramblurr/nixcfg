{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-ipmi-exporter;
in
{

  options.modules.telemetry.prometheus-ipmi-exporter = {
    enable = lib.mkEnableOption "prometheus-ipmi-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.ipmi = {
      enable = true;
      openFirewall = true;
    };
  };
}
