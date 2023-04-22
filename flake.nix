{
  inputs = {
    hyprland.url = "github:hyprwm/Hyprland";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    impermanence.url = "github:nix-community/impermanence";
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    sops-nix = {url = "github:Mic92/sops-nix/master";};
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland/master";
      inputs."nixpkgs".follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    alejandra,
    self,
    nixpkgs,
    hyprland,
    home-manager,
    nur,
    nixos-hardware,
    nixpkgs-wayland,
    sops-nix,
    ...
  }: {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    nixosConfigurations = {
      quine = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/quine/configuration.nix
          nur.nixosModules.nur
          home-manager.nixosModules.home-manager
          hyprland.nixosModules.default
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          {programs.hyprland.enable = true;}
        ];
      };
    };
  };
}
