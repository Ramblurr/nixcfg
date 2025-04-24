# This is from Tristan Maat @TLATER
# source: https://github.com/TLATER/dotfiles/tree/32489827a2a85ac2d6584e84a11a0da37bdc19dc/nixos-modules/nvidia
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.hardware.easyNvidia;
in
{
  imports = [ ./vaapi.nix ];

  options.modules.hardware.easyNvidia = with lib.types; {
    enable = lib.mkEnableOption "easyNvidia";
    withIntegratedGPU = lib.mkOption {
      type = bool;
      description = ''
        Whether the computer has a separate integrated GPU.

        This also configures the machine to use the integrated GPU for
        other things like software decoding, so keep this enabled even
        if you separately disable offload rendering.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = {
      package =
        config.boot.kernelPackages.nvidiaPackages.mkDriver
          #   {
          #   version = "570.124.04";
          #   sha256_64bit = "sha256-G3hqS3Ei18QhbFiuQAdoik93jBlsFI2RkWOBXuENU8Q=";
          #   sha256_aarch64 = lib.fakeHash;
          #   openSha256 = "sha256-KCGUyu/XtmgcBqJ8NLw/iXlaqB9/exg51KFx0Ta5ip0=";
          #   settingsSha256 = "sha256-LNL0J/sYHD8vagkV1w8tb52gMtzj/F0QmJTV1cMaso8=";
          #   persistencedSha256 = lib.fakeHash;
          # };
          {
            version = "570.144";
            sha256_64bit = "sha256-wLjX7PLiC4N2dnS6uP7k0TI9xVWAJ02Ok0Y16JVfO+Y=";
            sha256_aarch64 = lib.fakeSha256;
            openSha256 = "";
            settingsSha256 = "sha256-VcCa3P/v3tDRzDgaY+hLrQSwswvNhsm93anmOhUymvM=";
            persistencedSha256 = lib.fakeSha256;
          };

      # Power management is nearly always required to get nvidia GPUs to
      # behave on suspend, due to firmware bugs.
      powerManagement.enable = true;
      # The open driver is recommended by nvidia now, see
      # https://download.nvidia.com/XFree86/Linux-x86_64/565.57.01/README/kernel_open.html
      open = false;

      dynamicBoost.enable = cfg.enable && cfg.withIntegratedGPU;
    };

    boot = {
      #kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_xanmod;
      kernelParams = [
        #"video=3440x1440@100" # for virtual console resolution
        "quiet"
        "splash"
        "boot.shell_on_fail"
        "nvidia"
        "nvidia_modeset"
        "nvidia-uvm"
        "nvidia_drm"
      ];
      extraModprobeConfig =
        "options nvidia "
        + lib.concatStringsSep " " [
          # nvidia assume that by default your CPU does not support PAT,
          # but this is effectively never the case in 2023
          "NVreg_UsePageAttributeTable=1"
          # This is sometimes needed for ddc/ci support, see
          # https://www.ddcutil.com/nvidia/
          #
          # Current monitor does not support it, but this is useful for
          # the future
          #"NVreg_RegistryDwords=RMUseSwI2c=0x01;RMI2cSpeed=100"
        ];
    };

    # No longer setting these here, and instead setting them on a per-app basis as needed
    #environment.variables = {
    #  # Required to run the correct GBM backend for nvidia GPUs on wayland
    #  GBM_BACKEND = "nvidia-drm";
    #  # Apparently, without this nouveau may attempt to be used instead
    #  # (despite it being blacklisted)
    #  __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    #  # Hardware cursors are currently broken on wlroots
    #  WLR_NO_HARDWARE_CURSORS = "1";
    #};
    environment.systemPackages = [
      pkgs.nvitop
      pkgs.nvtopPackages.nvidia
      pkgs.libva-utils
      pkgs.cudaPackages.cudatoolkit
    ];
  };
}
