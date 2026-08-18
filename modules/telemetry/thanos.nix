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
      "thanos-object-storage-access-key" = { };
      "thanos-object-storage-secret-key" = { };
    };
    sops.templates."thanos-object-storage.yaml" = {
      owner = "root";
      group = "thanos-objstore";
      mode = "0440";
      content = ''
        type: S3
        config:
          bucket: debord-thanos
          endpoint: garage.mgmt.${config.repo.secrets.global.domain.home}
          region: us-east-1
          access_key: ${config.sops.placeholder."thanos-object-storage-access-key"}
          secret_key: ${config.sops.placeholder."thanos-object-storage-secret-key"}
          insecure: false
          signature_version2: false
          bucket_lookup_type: path
      '';
    };

    services.thanos = {
      sidecar = {
        enable = true;
        objstore.config-file = config.sops.templates."thanos-object-storage.yaml".path;
        grpc-address = "127.0.0.1:10901";
        http-address = "127.0.0.1:10902";
      };

      store = {
        enable = true;
        objstore.config-file = config.sops.templates."thanos-object-storage.yaml".path;
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
        objstore.config-file = config.sops.templates."thanos-object-storage.yaml".path;
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
