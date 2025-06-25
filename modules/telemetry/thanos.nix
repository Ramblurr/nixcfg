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
    sops.secrets = {
      thanos_sidecar_object_storage_configuration = {
        format = "yaml";
        mode = "0777";
        owner = "prometheus";
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
      };
    };
  };
}
