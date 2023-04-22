{
  config,
  lib,
  pkgs,
  options,
  ...
}: let
  cfg = config.nixcfg.ryzen;
in {
  options = {
    nixcfg.ryzen = {
      undervolt = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };

        value = lib.mkOption {
          type = lib.types.str;
          default = "-p 0 -v 30 -f A8"; # Pstate 0, 1.25 voltage, 4200 clock speed
        };
      };
    };
  };
  config = {
    boot.kernelModules = ["msr"]; # Needed for zenstates
    hardware.cpu.amd.updateMicrocode = true;
    # Ryzen cpu control
    systemd.services.zenstates = lib.mkIf cfg.undervolt.enable {
      enable = true;
      description = "Ryzen Undervolt";
      after = ["syslog.target" "systemd-modules-load.service"];

      unitConfig = {
        ConditionPathExists = "${pkgs.zenstates}/bin/zenstates";
      };

      serviceConfig = {
        User = "root";
        ExecStart = "${pkgs.zenstates}/bin/zenstates ${cfg.undervolt.value}";
      };

      wantedBy = ["multi-user.target"];
    };
  };
}
