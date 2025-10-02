{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.modules.services.powerdns;
  configDir = pkgs.writeTextDir "pdns.conf" "${cfg.extraConfig}";
  finalConfigDir = if cfg.secretFile == null then configDir else "/run/pdns";
in
{
  options = {
    modules.services.powerdns = {
      enable = mkEnableOption "PowerDNS domain name server";

      package = lib.mkOption {
        type = lib.types.package;
      };

      extraConfig = mkOption {
        type = types.lines;
        default = "launch=bind";
        description = ''
          PowerDNS configuration. Refer to
          <https://doc.powerdns.com/authoritative/settings.html>
          for details on supported values.
        '';
      };

      secretFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/run/keys/powerdns.env";
        description = ''
          Environment variables from this file will be interpolated into the
          final config file using envsubst with this syntax: `$ENVIRONMENT`
          or `''${VARIABLE}`.
          The file should contain lines formatted as `SECRET_VAR=SECRET_VALUE`.
          This is useful to avoid putting secrets into the nix store.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    environment.etc.pdns.source = finalConfigDir;

    environment.systemPackages = [ cfg.package ];
    systemd.packages = [ cfg.package ];

    systemd.services.pdns = {
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "mysql.service"
        "postgresql.target"
        "openldap.service"
      ];

      serviceConfig = {
        EnvironmentFile = lib.optional (cfg.secretFile != null) cfg.secretFile;
        ExecStartPre = lib.optional (cfg.secretFile != null) (
          pkgs.writeShellScript "pdns-pre-start" ''
            umask 077
            ${pkgs.envsubst}/bin/envsubst -i "${configDir}/pdns.conf" > ${finalConfigDir}/pdns.conf
          ''
        );
        ExecStart = [
          ""
          "${cfg.package}/bin/pdns_server --config-dir=${finalConfigDir} --guardian=no --daemon=no --disable-syslog --log-timestamp=no --write-pid=no"
        ];
      };
    };

    users.users.pdns = {
      isSystemUser = true;
      group = "pdns";
      description = "PowerDNS";
    };

    users.groups.pdns = { };

  };
}
