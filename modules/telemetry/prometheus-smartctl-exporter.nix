{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus-smartctl-exporter;
in
{
  options.modules.telemetry.prometheus-smartctl-exporter = {
    enable = lib.mkEnableOption "prometheus-smartctl-exporter";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.smartctl = {
      enable = true;
      openFirewall = true;
    };

    # Required for SMART to access NVMe disks
    # https://github.com/NixOS/nixpkgs/issues/210041#issuecomment-1694704611
    services.udev.extraRules = ''
      SUBSYSTEM=="nvme", KERNEL=="nvme[0-9]*", GROUP="disk"
    '';
  };
}
