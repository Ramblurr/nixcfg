{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.grafana;
  stateDir = "/var/lib/grafana";

  serviceDeps = [
    "var-lib-grafana.mount"
    "zfs-datasets.service"
  ];
  grafanaDomain = cfg.domain;
in
{
  options.modules.services.grafana = {
    enable = lib.mkEnableOption "grafana";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "code.example.com";
      description = "The domain to use for the forgejo";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };

  config = lib.mkIf cfg.enable {

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/grafana"."mountpoint" = stateDir;
    };

    systemd.tmpfiles.rules = [
      "z '${stateDir}' 750 grafana grafana - -"
    ];

    systemd.services.grafana = {
      requires = serviceDeps;
      after = serviceDeps;
    };

    services.grafana = {
      enable = true;

      settings = {
        smtp = {
          enabled = true;
          host = "${config.repo.secrets.global.email.siteRelay}:25";
          from_address = config.repo.secrets.global.email.home;
        };
        server = {
          domain = grafanaDomain;
          root_url = "https://${grafanaDomain}";
          protocol = "socket";
        };

        panels = {
          enable_alpha = "true";
          disable_sanitize_html = "true";
        };
      };
      declarativePlugins = with pkgs.grafanaPlugins; [
        grafana-piechart-panel
        grafana-clock-panel
        frser-sqlite-datasource
      ];
    };
  };

}
