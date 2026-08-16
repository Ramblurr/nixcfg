{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.thanos;
in
{
  options.modules.telemetry.thanos = {
    enable = lib.mkEnableOption "thanos";
  };

  config = lib.mkIf cfg.enable {
    users.groups.thanos-objstore = { };
    sops.secrets = {
      thanos_sidecar_object_storage_configuration = {
        format = "yaml";
        mode = "0440";
        owner = "root";
        group = "thanos-objstore";
      };
    };

    services.thanos = {
      sidecar = {
        enable = true;
        objstore.config-file = config.sops.secrets.thanos_sidecar_object_storage_configuration.path;
        grpc-address = "127.0.0.1:10901";
        http-address = "127.0.0.1:10902";
      };

      store = {
        enable = true;
        objstore.config-file = config.sops.secrets.thanos_sidecar_object_storage_configuration.path;
        grpc-address = "127.0.0.1:10903";
        http-address = "127.0.0.1:10904";
      };

      query = {
        enable = true;
        grpc-address = "127.0.0.1:10905";
        http-address = "127.0.0.1:10906";
        endpoints = [
          config.services.thanos.sidecar.grpc-address
          config.services.thanos.store.grpc-address
        ];
      };

      compact = {
        enable = true;
        objstore.config-file = config.sops.secrets.thanos_sidecar_object_storage_configuration.path;
        http-address = "127.0.0.1:10907";
        retention = {
          resolution-raw = "30d";
          resolution-5m = "1y";
          resolution-1h = "2y";
        };
      };
    };

    systemd.services =
      lib.genAttrs
        [
          "thanos-sidecar"
          "thanos-store"
          "thanos-compact"
        ]
        (_: {
          serviceConfig.SupplementaryGroups = [ "thanos-objstore" ];
        });
  };
}
