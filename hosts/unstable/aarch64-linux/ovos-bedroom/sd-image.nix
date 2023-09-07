{
  self,
  nixos-raspberrypi,
  ...
}: {
  sd-image =
    (self.nixosConfigurations.ovos-bedroom.extendModules {
      modules = [
        nixos-raspberrypi.nixosModules.sd-image-rpi3
        {
          system.nixos.variant_id = "ovos-sat";
          sdImage.imageBaseName = "ovos-bedroom-rpi3-nix-sd-image";
        }
      ];
    })
    .config
    .system
    .build
    .sdImage;
}
