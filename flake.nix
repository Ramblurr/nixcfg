{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.05";

    talhelper.url = "github:budimanjojo/talhelper";
    talhelper.inputs.nixpkgs.follows = "nixpkgs";

    hyprland.url = "github:hyprwm/Hyprland";

    firefox-nightly.url = "github:colemickens/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.inputs.nixpkgs-stable.follows = "nixpkgs-stable";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland/master";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    hyprNStack = {
      url = "github:SiriusStarr/hyprNStack";
      inputs.hyprland.follows = "hyprland";
    };

    hy3 = {
      url = "github:outfoxxed/hy3";
      inputs.hyprland.follows = "hyprland";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";

    mkPkgs = pkgsArg: systemArg:
      import pkgsArg {
        system = systemArg;
        config.allowUnfree = true;
        overlays = [self.overlay] ++ (lib.attrValues self.overlays);
      };

    pkgs = mkPkgs nixpkgs system;

    lib = nixpkgs.lib.extend (self: super: {
      my = import ./lib {
        inherit pkgs inputs;
        lib = self;
      };
    });
  in {
    lib = lib.my;

    nixosModules = mapModulesRec ./modules import;

    nixosConfigurations =
      mapHosts ./hosts/unstable/x86_64-linux
      {
        nixpkgs = nixpkgs;
        unstable = nixpkgs;
        system = "x86_64-linux";
        mkPkgs = mkPkgs;
        home-manager = inputs.home-manager;
        #   }
        # // mapHosts ./hosts/stable/x86_64-linux
        #   {
        #     nixpkgs = inputs.nixpkgs-stable;
        #     unstable = nixpkgs;
        #     system = "x86_64-linux";
        #     mkPkgs = mkPkgs;
        #     home-manager = inputs.home-manager-stable;
        #   }
        # // mapHosts ./hosts/stable/aarch64-linux
        #   {
        #     nixpkgs = inputs.nixpkgs-stable;
        #     unstable = nixpkgs;
        #     system = "aarch64-linux";
        #     mkPkgs = mkPkgs;
        #     home-manager = inputs.home-manager-stable;
      };

    packages."${system}" = import ./packages {inherit pkgs;};

    overlay = final: prev: {
      my = self.packages."${system}";
    };

    overlays = mapModules ./overlays import;
  };
}
