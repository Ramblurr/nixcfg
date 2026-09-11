{ config, pkgs, ... }:
{
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = false;
    # The GTX 1070 Ti uses Pascal. R580 is the last NVIDIA driver series
    # that supports this GPU. Newer driver series cannot be used.
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    nvidiaSettings = false;
  };
  boot.kernelModules = [ "nvidia_uvm" ];
  nix.settings = {
    extra-substituters = [ "https://cache.flox.dev" ];
    extra-trusted-public-keys = [ "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs=" ];
  };

  modules.services.immich-machine-learning = {
    enable = true;
    gpu = true;
    package = pkgs.callPackage ../../pkgs/immich-machine-learning-pascal.nix { };
  };
}
