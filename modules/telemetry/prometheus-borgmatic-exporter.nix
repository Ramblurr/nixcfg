{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-borgmatic-exporter;
in
{
  options.modules.telemetry.prometheus-borgmatic-exporter = {
    enable = lib.mkEnableOption "prometheus-borgmatic-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.borgmatic = {
      enable = true;
      openFirewall = true;

      configFile = "/etc/borgmatic.d/";
    };

    systemd.services.prometheus-borgmatic-exporter.serviceConfig = {
      User = lib.mkForce "borgmatic";
      Group = lib.mkForce "borgmatic";
    };
  };
}
