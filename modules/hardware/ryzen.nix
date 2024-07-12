{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.hardware.ryzen;
in
{
  options = {
    modules.hardware.ryzen = {
      enable = lib.mkEnableOption "";
      undervolt = {
        enable = lib.mkEnableOption "";
        value = mkOption {
          type = types.str;
          default = "-p 0 -v 30 -f A8"; # Pstate 0, 1.25 voltage, 4200 clock speed
        };
      };
    };
  };

  config = mkIf cfg.enable {
    boot.kernelModules = [ "msr" ]; # Needed for zenstates
    hardware.cpu.amd.updateMicrocode = true;
    # Ryzen cpu control
    systemd.services.zenstates = mkIf cfg.undervolt.enable {
      enable = true;
      description = "Ryzen Undervolt";
      after = [
        "syslog.target"
        "systemd-modules-load.service"
      ];

      unitConfig = {
        ConditionPathExists = "${pkgs.zenstates}/bin/zenstates";
      };

      serviceConfig = {
        User = "root";
        ExecStart = "${pkgs.zenstates}/bin/zenstates ${cfg.undervolt.value}";
      };

      wantedBy = [ "multi-user.target" ];
    };
  };
}
