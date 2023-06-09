{
  config,
  pkgs,
  lib,
  ...
}: {
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    nvidia = {
      modesetting.enable = true; # Required for wayland
      # options: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/os-specific/linux/nvidia-x11/default.nix
      # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
      package = config.boot.kernelPackages.nvidiaPackages.production;
      # package = config.boot.kernelPackages.nvidiaPackages.beta;
      # NVreg_PreserveVideoMemoryAllocations=1
      powerManagement.enable = true;
    };

    uinput.enable = true;
  };
  boot.kernelParams = [
    "clearcpuid=514" # Fixes certain wine games crash on launch
    #"video=3440x1440@100" # for virtual console resolution
    "quiet"
    "splash"
    "boot.shell_on_fail"
    "nvidia"
    "nvidia_modeset"
    "nvidia-uvm"
    "nvidia_drm"
  ];

  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "nvidia";

    # maybe causes firefox crashed?
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    WLR_NO_HARDWARE_CURSORS = "1";
  };

  boot.kernel.sysctl = {"vm.max_map_count" = 2147483642;};
  boot.blacklistedKernelModules = ["nouveau" "mt7921e"];
  services.xserver.videoDrivers = ["nvidia"]; # Install the nvidia drivers
  virtualisation.docker.enableNvidia = true; # Enable nvidia gpu acceleration for docker
  environment.systemPackages = [
    pkgs.nvitop
    pkgs.nvtop-nvidia
    pkgs.libva-utils
  ];
}
