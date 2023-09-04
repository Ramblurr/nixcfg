{
  self,
  nixos-raspberrypi,
  ...
}: {
  sd-image =
    (self.nixosConfigurations.ovos-kitchen-sat.extendModules {
      modules = [
        nixos-raspberrypi.nixosModules.sd-image-rpizero2
        {
          system.nixos.variant_id = "ovos-sat";
          sdImage.imageBaseName = "ovos-kitchen-sat-rpizero2-nix-sd-image";
        }
      ];
    })
    .config
    .system
    .build
    .sdImage;
}
