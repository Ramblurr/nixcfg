{ config, lib, pkgs, ... }:
let
  ml = pkgs.callPackage ../../pkgs/immich-machine-learning-pascal.nix { };
in
{
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    nvidiaSettings = false;
  };
  boot.kernelModules = [ "nvidia_uvm" ];
  nix.settings = {
    extra-substituters = [ "https://cache.flox.dev" ];
    extra-trusted-public-keys = [ "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs=" ];
  };

  # Isolated ML validation endpoint; the production worker stays on Quine.
  services.immich = {
    enable = true;
    package = pkgs.immich.override { immich-machine-learning = ml; };
    database = { enable = false; createDB = false; };
    redis.enable = false;
    accelerationDevices = null;
    machine-learning.environment = {
      IMMICH_HOST = lib.mkForce "127.0.0.1";
      MACHINE_LEARNING_REQUEST_THREADS = "1";
      MACHINE_LEARNING_MODEL_INTER_OP_THREADS = "1";
      MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
      MACHINE_LEARNING_WORKER_TIMEOUT = lib.mkForce "300";
    };
  };
  systemd.services.immich-server.enable = false;
  systemd.services.postgresql-setup.enable = false;
  users.users.immich.extraGroups = [ "video" "render" ];
  systemd.services.immich-machine-learning.environment.LD_LIBRARY_PATH = "/run/opengl-driver/lib";
  environment.systemPackages = [ ml.validationPython pkgs.jellyfin-ffmpeg ];
  # Separate experiment: keep its Python/Torch stack out of Immich's environment.
  system.extraDependencies = [ (pkgs.callPackage ../../pkgs/pascal-reranker-python.nix { }) ];
}
