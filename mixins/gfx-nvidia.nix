{
  config,
  pkgs,
  lib,
  ...
}: {
  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
    uinput.enable = true;
    nvidia.modesetting.enable = true; # Required for wayland
  };
  kernelParams = [
    "clearcpuid=514" # Fixes certain wine games crash on launch
    "video=3440x1440@100" # for virtual console resolution
    #"quiet"
    "splash"
    "boot.shell_on_fail"
  ];

  kernel.sysctl = {"vm.max_map_count" = 262144;}; # Fixes crash when loading maps in CS2
  boot.blacklistedKernelModules = ["nouveau" "mt7921e"];
  services.xserver.videoDrivers = ["nvidia"]; # Install the nvidia drivers
  virtualisation.docker.enableNvidia = true; # Enable nvidia gpu acceleration for docker
  environment.systemPackages = [pkgs.nvtop-nvidia]; # Monitoring tool for nvidia GPUs
}
