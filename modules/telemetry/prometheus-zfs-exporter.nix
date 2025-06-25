{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-zfs-exporter;
in
{
  options.modules.telemetry.prometheus-zfs-exporter = {
    enable = lib.mkEnableOption "prometheus-zfs-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.zfs = {
      enable = true;
      openFirewall = true;
    };
  };
}
