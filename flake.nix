{
  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-mine.url = "github:ramblurr/nixpkgs/consolidated";
    #nixpkgs-mine.url = "path:/home/ramblurr/src/nixpkgs";

    nixfmt.url = "github:serokell/nixfmt";

    ags.url = "github:Aylur/ags";
    ags.inputs.nixpkgs.follows = "nixpkgs-unstable";

    firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    firefox-gnome-theme.flake = false;

    matugen.url = "github:InioX/matugen";
    matugen.inputs.nixpkgs.follows = "nixpkgs-unstable";

    radicle.url = "git+https://seed.radicle.xyz/z3gqcJUoA1n9HaHKufZs5FCSGazv5.git?rev=54aacc96197a48b79fcc260f94312d824f5e0a34";
    radicle.inputs.nixpkgs.follows = "nixpkgs-unstable";
    radicle.inputs.flake-utils.follows = "flake-utils";

    quadlet-nix.url = "github:Ramblurr/nixos-quadlet/feat-home-manager";
    quadlet-nix.inputs.nixpkgs.follows = "nixpkgs-unstable";
    quadlet-nix.inputs.home-manager.follows = "home-manager";

    nixos-raspberrypi.url = "github:ramblurr/nixos-raspberrypi";
    nixos-raspberrypi.inputs.nixpkgs.follows = "nixpkgs-unstable";

    nixos-raspberrypi-stable.url = "github:ramblurr/nixos-raspberrypi/dev";
    nixos-raspberrypi-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    nixos-ovos.url = "github:ramblurr/ovos-rpi-nixos/dev";
    nixos-ovos.inputs.nixpkgs.follows = "nixpkgs-unstable";
    nixos-ovos.inputs.nixos-raspberrypi.follows = "nixos-raspberrypi";

    flake-utils.url = "github:numtide/flake-utils";

    disko-unstable.url = "github:nix-community/disko";
    disko-unstable.inputs.nixpkgs.follows = "nixpkgs-unstable";

    disko-stable.url = "github:nix-community/disko";
    disko-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    hyprland.url = "github:hyprwm/Hyprland/v0.37.1";

    #hyprNStack.url = "github:SiriusStarr/hyprNStack";
    #hyprNStack.inputs.hyprland.follows = "hyprland";

    #hy3.url = "github:outfoxxed/hy3/hl0.37.1";
    #hy3.inputs.hyprland.follows = "hyprland";

    #fast-flake-update.url = "github:Mic92/fast-flake-update";
    fast-flake-update.url = "github:zi3m5f/fast-flake-update/use_git_worktree_cmd";
    fast-flake-update.inputs.nixpkgs.follows = "nixpkgs-unstable";

    firefox-nightly.url = "github:colemickens/flake-firefox-nightly";
    firefox-nightly.inputs.nixpkgs.follows = "nixpkgs-unstable";

    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs-unstable";

    impermanence.url = "github:nix-community/impermanence";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs-unstable";
    sops-nix.inputs.nixpkgs-stable.follows = "nixpkgs-stable";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland/master";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager-stable.url = "github:nix-community/home-manager/release-23.11";
    home-manager-stable.inputs.nixpkgs.follows = "nixpkgs-stable";

    nur.url = "github:nix-community/NUR";

    attic.url = "github:zhaofengli/attic";
    attic.inputs.nixpkgs.follows = "nixpkgs-unstable";
    attic.inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    attic.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    inputs@{
      self,
      nixpkgs-unstable,
      nixos-raspberrypi,
      nixos-raspberrypi-stable,
      ...
    }:
    let
      defaultSystem = "x86_64-linux";

      allOverlays = [ self.overlay ] ++ (lib.attrValues self.overlays);

      mkPkgs =
        pkgsArg: systemArg:
        import pkgsArg {
          system = systemArg;
          config.allowUnfree = true;
          config.permittedInsecurePackages = [
            "electron-25.9.0"
            "electron-24.8.6"
          ];
          overlays = allOverlays;
        };

      pkgs = mkPkgs nixpkgs-unstable defaultSystem;
      pkgs-stable = mkPkgs inputs.nixpkgs-stable defaultSystem;

      lib = nixpkgs-unstable.lib.extend (
        self: super: {
          my = import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        }
      );

      mapHosts =
        path: nixpkgsType: system:
        let
          table =
            {
              unstable = {
                isStable = false;
                nixpkgs = nixpkgs-unstable;
                home-manager = inputs.home-manager;
                extraModules = [
                  inputs.disko-unstable.nixosModules.disko
                ] ++ lib.my.mapModulesRec' ./modules-unstable import;
              };

              stable = {
                isStable = true;
                nixpkgs = inputs.nixpkgs-stable;
                home-manager = inputs.home-manager-stable;
                extraModules = [ inputs.disko-stable.nixosModules.disko ];
              };
            }
            .${nixpkgsType};

          thisPkgs = mkPkgs table.nixpkgs system;
          thisLib = table.nixpkgs.lib.extend (
            self: super: {
              my = import ./lib {
                inherit inputs;
                pkgs = thisPkgs;
                lib = self;
              };
            }
          );
        in
        thisLib.my.mapHosts path {
          isStable = table.isStable;
          extraModules = table.extraModules;
          nixosSystem = table.nixpkgs.lib.nixosSystem;
          unstable = mkPkgs inputs.nixpkgs-unstable system;
          mine = mkPkgs inputs.nixpkgs-mine system;
          system = system;
          home-manager = table.home-manager;
          overlays = allOverlays;
        };
    in
    {
      lib = lib.my;
      # Nobody should really be consuming my modules from this flake
      # nixosModules = lib.my.mapModulesRec ./modules import;

      nixosConfigurations =
        (mapHosts ./hosts/unstable/x86_64-linux "unstable" "x86_64-linux")
        // (mapHosts ./hosts/stable/x86_64-linux "stable" "x86_64-linux")
        // (mapHosts ./hosts/stable/aarch64-linux "stable" "aarch64-linux");

      images = {
        ovos-kitchen =
          (import ./hosts/unstable/aarch64-linux/ovos-kitchen/sd-image.nix {
            inherit self nixos-raspberrypi;
          }).sd-image;
        ovos-bedroom =
          (import ./hosts/unstable/aarch64-linux/ovos-bedroom/sd-image.nix {
            inherit self nixos-raspberrypi;
          }).sd-image;
        fairybox =
          (import ./hosts/stable/aarch64-linux/fairybox/sd-image.nix {
            inherit self nixos-raspberrypi-stable;
          }).sd-image;
      };

      # breaks `nix flake check` with
      #   error: flake attribute 'packages.x86_64-linux.packages' is not a derivation
      # packages."${system}" = import ./packages {inherit pkgs;};

      overlay = final: prev: { my = self.packages."${defaultSystem}"; };

      packages."${defaultSystem}" = import ./packages { inherit pkgs-stable; };

      overlays = lib.my.mapModules ./overlays import;
    };
}
