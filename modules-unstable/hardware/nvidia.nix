{
  options,
  config,
  lib,
  pkgs,
  actual-nixpkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.nvidia;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.hardware.nvidia = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    nixpkgs.config.cudaSupport = true;
    hardware = {
      graphics.enable = true;
      graphics.enable32Bit = true; # required for virtualisation.docker.enableNvidia
      #opengl = {
      #  enable = true;
      #};
      nvidia = {
        nvidiaSettings = true; # Enable `nvidia-settings`
        modesetting.enable = true; # Required for wayland

        powerManagement.enable = false; # Disable to prevent suspend/resume issues
        open = false;
        # Driver version options
        # options: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/os-specific/linux/nvidia-x11/default.nix
        # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
        #package = config.boot.kernelPackages.nvidiaPackages.production;
        package = config.boot.kernelPackages.nvidiaPackages.beta; # as of Aug 2024 this is 560.35.03 which has wayland fixes
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
      # No longer setting these here, and instead setting them on a per-app basis as needed
      # GBM_BACKEND = "nvidia-drm";
      # LIBVA_DRIVER_NAME = "nvidia";
      # __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      # WLR_NO_HARDWARE_CURSORS = "1";
    };

    boot.kernel.sysctl = {
      "vm.max_map_count" = 2147483642;
    };
    boot.blacklistedKernelModules = [
      "nouveau"
      "mt7921e"
    ];
    services.xserver.videoDrivers = [ "nvidia" ]; # Install the nvidia drivers
    virtualisation.docker.enableNvidia = true; # Enable nvidia gpu acceleration for docker
    hardware.nvidia-container-toolkit.enable = true; # Enable nvidia gpu acceleration for docker
    environment.systemPackages = [
      pkgs.nvitop
      pkgs.nvtopPackages.nvidia
      pkgs.libva-utils
      pkgs.cudaPackages.cudatoolkit
    ];

    # force use of up to date nixos packages egl-wayland library, not the old one bundled in the nvidia driver
    # ref: https://github.com/NixOS/nixpkgs/issues/202454#issuecomment-1657230960
    # this makes hardware acceleration in firefox work!
    environment.etc =
      let
        mkEglFile =
          n: library:
          let
            suffix = lib.optionalString (library != "wayland") ".1";
            pkg = if library != "wayland" then config.hardware.nvidia.package else pkgs.egl-wayland;

            fileName = "${toString n}_nvidia_${library}.json";
            library_path = "${pkg}/lib/libnvidia-egl-${library}.so${suffix}";
          in
          {
            "egl/egl_external_platform.d/${fileName}".source = pkgs.writeText fileName (
              builtins.toJSON {
                file_format_version = "1.0.0";
                ICD = {
                  inherit library_path;
                };
              }
            );
          };
      in
      {
        "egl/egl_external_platform.d".enable = false;
      }
      // mkEglFile 10 "wayland"
      // mkEglFile 15 "gbm"
      // mkEglFile 20 "xcb"
      // mkEglFile 20 "xlib";

    # force use of up to date nixos packages egl-wayland library, not the old one bundled in the nvidia driver
    # ref: https://github.com/NixOS/nixpkgs/issues/202454#issuecomment-1657230960
    # this makes hardware acceleration in firefox work!
    #environment.etc."egl/egl_external_platform.d".source =
    #  let
    #    nvidia_wayland = pkgs.writeText "10_nvidia_wayland.json" ''
    #      {
    #          "file_format_version" : "1.0.0",
    #          "ICD" : {
    #              "library_path" : "${
    #                actual-nixpkgs.legacyPackages.${pkgs.hostPlatform.system}.egl-wayland
    #              }/lib/libnvidia-egl-wayland.so"
    #          }
    #      }
    #    '';
    #    nvidia_gbm = pkgs.writeText "15_nvidia_gbm.json" ''
    #      {
    #          "file_format_version" : "1.0.0",
    #          "ICD" : {
    #              "library_path" : "${config.hardware.nvidia.package}/lib/libnvidia-egl-gbm.so.1"
    #          }
    #      }
    #    '';
    #  in
    #  lib.mkForce (
    #    pkgs.runCommandLocal "nvidia-egl-hack" { } ''
    #      mkdir -p $out
    #      cp ${nvidia_wayland} $out/10_nvidia_wayland.json
    #      cp ${nvidia_gbm} $out/15_nvidia_gbm.json
    #    ''
    #  );
  };
}
