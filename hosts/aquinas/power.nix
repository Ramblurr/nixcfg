{
  config,
  lib,
  pkgs,
  ...
}:

{

  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = false;
  services.power-profiles-daemon.enable = false;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "performance";
      turbo = "auto";
    };
  };
  hardware = {
    graphics.enable = true;
    nvidia = {
      nvidiaSettings = true;
      modesetting.enable = true; # Required for wayland
      package = config.boot.kernelPackages.nvidiaPackages.production;
      powerManagement.enable = false; # Disable to prevent suspend/resume issues
      open = true;
      prime = {
        # Make sure to use the correct Bus ID values for your system!
        # https://nixos.wiki/wiki/Nvidia
        # sudo lshw -c display
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:01:0:0";
        sync.enable = true;
      };
    };
    uinput.enable = true;
  };
  environment.systemPackages = [
    pkgs.nvitop
    pkgs.nvtopPackages.nvidia
    pkgs.libva-utils
  ];

}
