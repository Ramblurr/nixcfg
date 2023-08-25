{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    talhelper.url = "github:budimanjojo/talhelper";
    talhelper.inputs.nixpkgs.follows = "nixpkgs";

    hyprland.url = "github:hyprwm/Hyprland";

    firefox-nightly.url ="github:colemickens/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";

    #nur.url = "github:nix-community/NUR";

    impermanence.url = "github:nix-community/impermanence";

    #alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    #alejandra.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland/master";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    #alejandra,
    self,
    nixpkgs,
    talhelper,
    hyprland,
    home-manager,
    #nur,
    nixos-hardware,
    firefox-nightly,
    nixpkgs-wayland,
    sops-nix,
    nix-gaming,
    ...
  }: let
    #system = "x86_64-linux";
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
      (self: super: {
        element-desktop = super.element-desktop.overrideAttrs (e: rec {
          desktopItem = e.desktopItem.override (d: {
            # I only want my own custom thunderbird desktop files showing up
            noDisplay = true;
          });
          installPhase = builtins.replaceStrings ["${e.desktopItem}"] ["${desktopItem}"] e.installPhase;
        });
      })
    ];
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    nixosConfigurations = {
      quine = nixpkgs.lib.nixosSystem rec {
        #inherit system;
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          {nix.registry.nixpkgs.flake = nixpkgs;
           nix.nixPath = ["nixpkgs=flake:nixpkgs"];}
          {nixpkgs.overlays = overlays;}
          #nur.nixosModules.nur
          home-manager.nixosModules.home-manager
          hyprland.nixosModules.default
          nix-gaming.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          inputs.impermanence.nixosModules.impermanence
          #{environment.systemPackages = [alejandra.defaultPackage.${system}];}
          ./modules/device.nix
          ./hosts/quine/configuration.nix
        ];
      };
    };
  };
}
