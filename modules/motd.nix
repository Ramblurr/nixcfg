{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.motd;
  edn = {
    str = s: builtins.toJSON s;
    strList = xs: "[" + lib.concatMapStringsSep " " edn.str xs + "]";
  };
  configEdn = ''
    {:role ${edn.str cfg.role}
     :units ${edn.strList cfg.units}
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
      description = "Systemd units to always show. Failed units show regardless.";
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
