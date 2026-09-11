{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.modules.services.immich-worker;
in
{
  options.modules.services.immich-worker.enable =
    lib.mkEnableOption "the remote Immich background worker with GPU video transcoding";

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (import ./immich-common.nix {
        inherit
          config
          lib
          pkgs
          utils
          ;
      })
      {
        assertions = [
          {
            assertion = !config.services.postgresql.enable;
            message = "The pinned Immich worker role requires no local PostgreSQL: its upstream setup-unit workaround must not suppress another cluster.";
          }
        ];
        services.immich = {
          enable = true;
          host = "localhost";
          openFirewall = false;
          database = {
            enable = false;
            createDB = false;
          };
          redis.enable = false;
          # NVENC requires the NVIDIA device set, not only a DRM render node.
          accelerationDevices = null;
          environment.IMMICH_WORKERS_INCLUDE = "microservices";
          machine-learning.enable = false;
        };
        users.users.${config.services.immich.user}.extraGroups = [
          "video"
          "render"
        ];

        # nixpkgs 1eb89746 defines this even with database.enable = false.
        # Reject co-location above rather than deleting another cluster's hooks.
        systemd.services.postgresql-setup.enable = false;
        systemd.services.immich-server = {
          environment.LD_LIBRARY_PATH = "/run/opengl-driver/lib";
          serviceConfig = {
            Nice = 10;
            CPUWeight = 20;
            IOWeight = 20;
          };
        };
      }
    ]
  );
}
