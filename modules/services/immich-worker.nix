{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.modules.services.immich-worker;
  # Match the standard CUDA package set published by Flox, including its
  # default OpenVINO support. Keep this separate from the host package set.
  cudaPkgs = import pkgs.path {
    system = pkgs.stdenv.hostPlatform.system;
    config = {
      allowUnfree = true;
      cudaSupport = true;
    };
  };
  cudaPython = pkgs.python3.override (previous: {
    packageOverrides = lib.composeExtensions (previous.packageOverrides or (_: _: { })) (
      _: prev: {
        onnxruntime = prev.onnxruntime.override {
          onnxruntime = cudaPkgs.onnxruntime;
        };
      }
    );
  });
  gpuImmich = pkgs.immich.override {
    immich-machine-learning = pkgs.immich-machine-learning.override { python3 = cudaPython; };
  };
in
{
  options.modules.services.immich-worker.enable =
    lib.mkEnableOption "the remote Immich background worker and native CUDA ML role";

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
          package = lib.mkDefault gpuImmich;
          host = "localhost";
          openFirewall = false;
          database = {
            enable = false;
            createDB = false;
          };
          redis.enable = false;
          # CUDA requires /dev/nvidia* (including UVM), not only a DRM render node.
          accelerationDevices = null;
          environment.IMMICH_WORKERS_INCLUDE = "microservices";
          machine-learning = {
            enable = true;
            environment = {
              MACHINE_LEARNING_REQUEST_THREADS = "1";
              MACHINE_LEARNING_MODEL_INTER_OP_THREADS = "1";
              MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
              MACHINE_LEARNING_WORKER_TIMEOUT = lib.mkForce "300";
            };
          };
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
        systemd.services.immich-machine-learning = lib.mkIf config.services.immich.machine-learning.enable {
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
