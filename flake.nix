{
  inputs = {
    hyprland.url = "github:hyprwm/Hyprland";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-gaming.url = "github:fufexan/nix-gaming";
    #nur.url = "github:nix-community/NUR";
    impermanence.url = "github:nix-community/impermanence";
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    #nur,
    nixos-hardware,
    nixpkgs-wayland,
    sops-nix,
    nix-gaming,
    ...
  }: let
    overlays = [
      (self: super: {
        microsocks = super.pkgs.callPackage ./packages/microsocks {};
      })
      (self: super: {
        thunderbird = super.thunderbird.overrideAttrs (e: rec {
          desktopItem = e.desktopItem.override (d: {
            # I only want my own custom thunderbird desktop files showing up
            noDisplay = true;
          });
          buildCommand = builtins.replaceStrings ["${e.desktopItem}"] ["${desktopItem}"] e.buildCommand;
        });
      })
      (self: super: {
        firefox = super.firefox.overrideAttrs (e: rec {
          desktopItem = e.desktopItem.override (d: {
            # I only want my own custom firefox desktop files showing up
            noDisplay = true;
          });
          buildCommand = builtins.replaceStrings ["${e.desktopItem}"] ["${desktopItem}"] e.buildCommand;
        });
      })
    ];
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    nixosConfigurations = {
      quine = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          {nixpkgs.overlays = overlays;}
          #nur.nixosModules.nur
          home-manager.nixosModules.home-manager
          hyprland.nixosModules.default
          nix-gaming.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          inputs.impermanence.nixosModules.impermanence
          {environment.systemPackages = [alejandra.defaultPackage.${system}];}
          ./hosts/quine/configuration.nix
        ];
      };
    };
  };
}
