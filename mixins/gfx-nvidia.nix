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
      extraPackages = with pkgs; [vaapiVdpau nvidia-vaapi-driver];
    };
    nvidia = {
      modesetting.enable = true; # Required for wayland
      package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
    };

    uinput.enable = true;
  };
  boot.kernelParams = [
    "clearcpuid=514" # Fixes certain wine games crash on launch
    "video=3440x1440@100" # for virtual console resolution
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

  boot.kernel.sysctl = {"vm.max_map_count" = 262144;}; # Fixes crash when loading maps in CS2
  boot.blacklistedKernelModules = ["nouveau" "mt7921e"];
  services.xserver.videoDrivers = ["nvidia"]; # Install the nvidia drivers
  virtualisation.docker.enableNvidia = true; # Enable nvidia gpu acceleration for docker
  environment.systemPackages = [
    pkgs.nvitop
    pkgs.nvtop-nvidia
  ]; # Monitoring tool for nvidia GPUs
}
