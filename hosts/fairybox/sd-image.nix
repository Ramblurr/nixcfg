{ self, nixos-raspberrypi, ... }:
{
  sd-image =
    (self.nixosConfigurations.fairybox.extendModules {
      modules = [
        nixos-raspberrypi.nixosModules.sd-image-rpi4
        {
          system.nixos.variant_id = "fairybox";
          sdImage.imageBaseName = "fairybox-rpi4-nix-sd-image";
        }
      ];
    }).config.system.build.sdImage;
}
