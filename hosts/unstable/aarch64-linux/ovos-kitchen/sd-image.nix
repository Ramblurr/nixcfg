{ self, nixos-raspberrypi, ... }: {
  sd-image = (self.nixosConfigurations.ovos-kitchen.extendModules {
    modules = [
      nixos-raspberrypi.nixosModules.sd-image-rpi4
      {
        system.nixos.variant_id = "ovos";
        sdImage.imageBaseName = "ovos-kitchen-rpi4-nix-sd-image";
      }
    ];
  }).config.system.build.sdImage;
}
