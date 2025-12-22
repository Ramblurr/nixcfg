{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-nut-exporter;
in
{

  options.modules.telemetry.prometheus-nut-exporter = {
    enable = lib.mkEnableOption "prometheus-nut-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.nut = {
      enable = true;
      openFirewall = true;
      extraFlags = [ "--nut.vars_enable=''" ];
      nutUser = "admin";
      group = "nut";
      passwordPath = config.sops.secrets.upsAdminPassword.path;
    };
  };
}
