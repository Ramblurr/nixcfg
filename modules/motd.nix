{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.motd;
  units = lib.unique cfg.units;
  # A template instance such as getty@tty1.service is defined as getty@.service.
  templateOf =
    unit:
    let
      m = builtins.match "([^@]+@).*(\\.[a-z]+)" unit;
    in
    if m == null then null else lib.concatStrings m;
  unitDefined =
    unit:
    let
      template = templateOf unit;
    in
    (config.systemd.units.${unit}.enable or false)
    || (template != null && (config.systemd.units.${template}.enable or false));
  undefinedUnits = lib.filter (unit: !unitDefined unit) units;
  edn = {
    str = s: builtins.toJSON s;
    strList = xs: "[" + lib.concatMapStringsSep " " edn.str xs + "]";
  };
  configEdn = ''
    {:role ${edn.str cfg.role}
     :units ${edn.strList units}
     :disks ${if cfg.disks == null then "nil" else edn.strList cfg.disks}}
  '';
in
{
  options.modules.motd = {
    enable = lib.mkEnableOption "the motd login status summary for SSH sessions";
    role = lib.mkOption {
      type = lib.types.str;
      default = if (config.modules.microvm-guest.enable or false) then "microvm" else "host";
      defaultText = lib.literalExpression ''if config.modules.microvm-guest.enable then "microvm" else "host"'';
      description = "Role label shown in the motd header.";
    };
    units = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "caddy.service"
        "restic-backup.timer"
      ];
      description = "Systemd units to always show, each defined on this system. Duplicates are dropped. Failed units show regardless.";
    };
    disks = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
      default = null;
      example = [
        "/"
        "/persist"
      ];
      description = "Mount points to show, in order. Null shows every real mount.";
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.motd;
      defaultText = lib.literalExpression "pkgs.motd";
      description = "The motd package.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = undefinedUnits == [ ];
        message = "modules.motd.units names units this system does not define: ${lib.concatStringsSep ", " undefinedUnits}";
      }
    ];
    environment.systemPackages = [ cfg.package ];
    environment.etc."motd/config.edn".text = configEdn;
    environment.loginShellInit = ''
      if [ -n "''${SSH_CONNECTION:-}" ] && [ -t 1 ] && [ -z "''${MOTD_SHOWN:-}" ]; then
        export MOTD_SHOWN=1
        ${lib.getExe cfg.package} || true
      fi
    '';
    services.openssh.settings.PrintMotd = lib.mkDefault false;
  };
}
