{ inputs, ... }:
{
  flake =
    { config, lib, ... }:
    let
      mkHosts = (import ./nixos.nix { inherit inputs lib; }).mkHosts;

      hosts = {
        debord = {
          isStable = false;
          system = "x86_64-linux";
          path = ../hosts/debord;
          #hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        };
        dewey = {
          isStable = false;
          system = "x86_64-linux";
          path = ../hosts/dewey;
          hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        };
        aquinas = {
          isStable = false;
          system = "x86_64-linux";
          path = ../hosts/aquinas;
        };
        witt = {
          isStable = false;
          system = "x86_64-linux";
          path = ../hosts/witt;
        };
        quine = {
          isStable = false;
          system = "x86_64-linux";
          path = ../hosts/quine;
          hostOverlays = [ (import ../overlays/qemu.nix) ];
        };
        mali = {
          isStable = true;
          system = "x86_64-linux";
          path = ../hosts/mali;
        };
      };
    in
    {
      nixosConfigurations = mkHosts hosts;
    };
}
