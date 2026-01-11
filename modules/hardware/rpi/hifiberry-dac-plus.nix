{ lib, ... }:

{

  options = {
    modules.hardware.rpi.hifiberry-dac-plus = {
      enable = lib.mkEnableOption ''
        support for the Raspberry Pi Hifiberry DAC + HAT
      '';

    };
  };
  config = {
    hardware.alsa.enablePersistence = true;
    environment.etc."asound.conf".text = ''
      defaults.pcm.!card 1
      defaults.ctl.!card 1
    '';
    hardware.deviceTree = {
      enable = true;
      overlays = [
        {
          name = "hifiberry-dacplus";
          dtsFile = ./hifiberry-dacplus-overlay.dts;
        }
      ];
    };
    nixpkgs = {
      overlays = [
        # fix for kernel build failure:
        #
        #     modprobe: FATAL: Module ahci not found in directory
        #
        # https://github.com/NixOS/nixpkgs/issues/154163
        (_final: super: {
          makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
        })

        # fdtoverlay does not support the DTS overlay file I'm using above
        # (it exits with FDT_ERR_NOTFOUND)
        # https://github.com/raspberrypi/firmware/issues/1718
        #
        # we'll use dtmerge instead.
        # https://github.com/NixOS/nixos-hardware/blob/master/raspberry-pi/4/pkgs-overlays.nix
        (_final: prev: {
          deviceTree = prev.deviceTree // {
            applyOverlays = prev.callPackage ./apply-overlays-dtmerge.nix { };
          };
        })
      ];

    };
  };
}
