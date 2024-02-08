{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-bleeding-edge.url = "github:NixOS/nixpkgs/master";
    nixpkgs-oldstable.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";

    nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi";
    nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs";

    nixos-ovos.url = "github:ramblurr/ovos-rpi-nixos/dev";
    nixos-ovos.inputs.nixpkgs.follows = "nixpkgs";
    nixos-ovos.inputs.nixos-raspberrypi.follows = "nixos-raspberrypi";

    disko-stable.url = "github:nix-community/disko";
    disko-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

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

    home-manager-stable.url = "github:nix-community/home-manager/release-23.11";
    home-manager-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    hyprNStack = {
      url = "github:SiriusStarr/hyprNStack";
      inputs.hyprland.follows = "hyprland";
    };

    hy3 = {
      url = "github:outfoxxed/hy3";
      inputs.hyprland.follows = "hyprland";
    };
    nur = {
      url = "github:nix-community/NUR";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixos-raspberrypi,
    ...
  }: let
    defaultSystem = "x86_64-linux";

    allOverlays = [self.overlay] ++ (lib.attrValues self.overlays);

    mkPkgs = pkgsArg: systemArg:
      import pkgsArg {
        system = systemArg;
        config.allowUnfree = true;
        config.permittedInsecurePackages = [
          "electron-25.9.0"
          "electron-24.8.6"
        ];

        overlays = allOverlays;
      };

    pkgs = mkPkgs nixpkgs defaultSystem;
    pkgs-stable = mkPkgs inputs.nixpkgs-stable defaultSystem;

    lib = nixpkgs.lib.extend (self: super: {
      my = import ./lib {
        inherit pkgs inputs;
        lib = self;
      };
    });

    mapHosts = path: nixpkgsType: system: let
      table =
        {
          unstable = {
            nixpkgs = nixpkgs;
            home-manager = inputs.home-manager;
            extraModules = [];
          };

          stable = {
            nixpkgs = inputs.nixpkgs-stable;
            home-manager = inputs.home-manager-stable;
            extraModules = [inputs.disko-stable.nixosModules.disko];
          };
        }
        .${nixpkgsType};

      thisPkgs = mkPkgs table.nixpkgs system;
      thisLib = table.nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit inputs;
          pkgs = thisPkgs;
          lib = self;
        };
      });
    in
      thisLib.my.mapHosts path
      {
        extraModules = table.extraModules;
        nixosSystem = table.nixpkgs.lib.nixosSystem;
        unstable = mkPkgs inputs.nixpkgs system;
        system = system;
        edge = mkPkgs inputs.nixpkgs-bleeding-edge system;
        home-manager = table.home-manager;
        overlays = allOverlays;
      };
  in {
    lib = lib.my;
    nixosModules = lib.my.mapModulesRec ./modules import;

    nixosConfigurations =
      (mapHosts ./hosts/unstable/x86_64-linux "unstable" "x86_64-linux")
      // (mapHosts ./hosts/stable/x86_64-linux "stable" "x86_64-linux");
    #// (mapHosts ./hosts/stable/aarch64-linux "stable" "aarch64-linux");
    #mapHosts ./hosts/unstable/x86_64-linux
    #{
    #  nixpkgs = nixpkgs;
    #  unstable = nixpkgs;
    #  edge = inputs.nixpkgs-bleeding-edge;
    #  system = "x86_64-linux";
    #  mkPkgs = mkPkgs;
    #  home-manager = inputs.home-manager;
    #}
    #// mapHosts ./hosts/stable/x86_64-linux
    #{
    #  nixpkgs = inputs.nixpkgs-stable;
    #  unstable = nixpkgs;
    #  edge = inputs.nixpkgs-bleeding-edge;
    #  system = "x86_64-linux";
    #  mkPkgs = mkPkgs;
    #  home-manager = inputs.home-manager-stable;
    #}
    #// mapHosts ./hosts/unstable/aarch64-linux
    #{
    #  nixpkgs = nixpkgs;
    #  unstable = nixpkgs;
    #  edge = inputs.nixpkgs-bleeding-edge;
    #  system = "aarch64-linux";
    #  mkPkgs = mkPkgs;
    #  home-manager = inputs.home-manager-stable;
    #};

    images = {
      ovos-kitchen = (import ./hosts/unstable/aarch64-linux/ovos-kitchen/sd-image.nix {inherit self nixos-raspberrypi;}).sd-image;
      ovos-bedroom = (import ./hosts/unstable/aarch64-linux/ovos-bedroom/sd-image.nix {inherit self nixos-raspberrypi;}).sd-image;
    };

    # breaks `nix flake check` with
    #   error: flake attribute 'packages.x86_64-linux.packages' is not a derivation
    # packages."${system}" = import ./packages {inherit pkgs;};

    overlay = final: prev: {
      my = self.packages."${defaultSystem}";
    };

    packages."${defaultSystem}" = import ./packages {inherit pkgs-stable;};

    overlays = lib.my.mapModules ./overlays import;
  };
}
