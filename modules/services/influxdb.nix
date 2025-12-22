{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.influxdb;
  stateDirActual = "/var/lib/private/influxdb";
  stateDirEffective = "/var/lib/influxdb";
in
{
  options.modules.services.influxdb = {
    enable = lib.mkEnableOption "influxdb";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "influxdb.example.com";
      description = "The domain to use for the influxdb";
    };

    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
      };
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/influxdb"."mountpoint" = stateDirActual;
    };
    environment.systemPackages = with pkgs; [ influxdb2-cli ];
    systemd.services.influxdb2 = {
      unitConfig = {
        RequiresMountsFor = [ stateDirActual ];
      };
      serviceConfig = {
        DynamicUser = true;
        StateDirectory = lib.mkForce (baseNameOf stateDirEffective);
        User = lib.mkForce null;
        Group = lib.mkForce null;
      };
    };
    services.influxdb2 = {
      enable = true;
      settings = {
        reporting-disabled = true;
        http-bind-address = "127.0.0.1:${toString cfg.ports.http}";
      };
      provision = {
        enable = false;
      };
    };

    systemd.tmpfiles.rules = [
      "d '/persist${config.modules.users.primaryUser.homeDirectory}/.influxdbv2' - ${config.modules.users.primaryUser.username} ${config.modules.users.primaryUser.username} - -"
    ];
    myhm = {
      persistence = {
        directories = [ ".influxdbv2" ];
      };
    };
  };
}
