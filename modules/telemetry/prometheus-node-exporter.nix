{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-node-exporter;
in
{
  options.modules.telemetry.prometheus-node-exporter = {
    enable = lib.mkEnableOption "prometheus-node-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.node = {
      enable = true;
      openFirewall = true;
      enabledCollectors = [ "systemd" ];
      disabledCollectors = [ "textfile" ];
    };
  };
}
