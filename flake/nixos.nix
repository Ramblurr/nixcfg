{
  inputs,
  self,
  lib,
  config,
  ...
}:

let

  defaultModules = [
    (import ../modules/default.nix)
    inputs.quadlet-nix2.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    inputs.sops-nix.nixosModules.sops
    inputs.microvm.nixosModules.host
  ];
  unstableDefaultModules = [
    inputs.disko-unstable.nixosModules.disko
    (import ../modules-unstable/default.nix)
  ];
  stableDefaultModules = [ inputs.disko-stable.nixosModules.disko ];

  defaultOverlays = [
    inputs.nixos-extra-modules.overlays.default
    #inputs.emacs-overlay.overlays.default
    (import ../lib inputs)
    (import ../overlays/last-known-good.nix)
    (import ../overlays/nixpkgs-mine-packages.nix inputs)
    (import ../overlays/overrides.nix)
  ]
  ++ (import ../pkgs/default.nix inputs);

  mkPkgs =
    {
      system,
      flake,
      overlays ? [ ],
    }:
    import flake {
      inherit system overlays;
      config = {
        #allowAliases = false;
        allowUnfree = true;
        permittedInsecurePackages = [
          "libsoup-2.74.3"
          "olm-3.2.16"
        ];
      };
    };

  mkHost =
    name:
    {
      isStable ? false,
      # isRpi not useable until https://github.com/nvmd/nixos-raspberrypi/issues/90
      isRpi ? false,
      system ? "x86_64-linux",
      hostPath ? ../hosts/${name},
      hostExtraModules ? [ ],
      hostOverlays ? [ ],
      enableDefaultModules ? true,
    }:
    let
      allOverlays = hostOverlays ++ defaultOverlays;
      actual-nixpkgs = if isStable then inputs.nixpkgs-stable else inputs.nixpkgs;
      actual-home-manager = if isStable then inputs.home-manager-stable else inputs.home-manager;
      nixpkgs' = mkPkgs {
        inherit system;
        flake = actual-nixpkgs;
        overlays = allOverlays;
      };
      nixpkgs-unstable = mkPkgs {
        inherit system;
        overlays = allOverlays;
        flake = inputs.nixpkgs;
      };
      nixosSystem =
        if isRpi then inputs.nixos-raspberrypi.lib.nixosSystem else actual-nixpkgs.lib.nixosSystem;
    in
    actual-nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        #inputs.nixpkgs.nixosModules.readOnlyPkgs
        {
          node.secretsDir = hostPath + "/secrets";
          node.name = name;
          nixpkgs.pkgs = nixpkgs';
        }
        hostPath
        actual-home-manager.nixosModules.home-manager
      ]
      ++ lib.optionals enableDefaultModules defaultModules
      ++ lib.optionals isStable stableDefaultModules
      ++ lib.optionals (!isStable) unstableDefaultModules
      ++ hostExtraModules;

      specialArgs = {
        inherit inputs self;
        inherit (config) nodes;
        inherit actual-nixpkgs;
        unstable = nixpkgs-unstable;
        inherit (nixpkgs') lib;
        hostName = name;
      };
    };

  mkGuest =
    name:
    {
      system ? "x86_64-linux",
      hostPath ? ../guests/${name},
      extraModules ? [ ],
    }:
    let
      pkgs = mkPkgs {
        inherit system;
        flake = inputs.nixpkgs;
        overlays = defaultOverlays;
      };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        # Use the correct instance lib that has our overlays
        inherit (pkgs) lib;
        inherit (config) nodes;
        inherit inputs;
        inherit (inputs) nixpkgs;
      };
      modules = [
        inputs.home-manager.nixosModules.home-manager
        inputs.microvm.nixosModules.microvm
        inputs.impermanence.nixosModules.impermanence
        inputs.sops-nix.nixosModules.sops
        ../config/guests.nix
        ../modules/shell/zsh
        ../modules/shell/atuin.nix
        {
          node.secretsDir = hostPath + "/secrets";
          sops.defaultSopsFile = hostPath + "/secrets.sops.yaml";
          node.name = name;
          nixpkgs.config.allowUnfree = true;
          modules.microvm-guest.enable = true;
        }
        ../guests/${name}
      ]
      ++ extraModules;
    };

  mkMkHosts =
    fn: hosts:
    lib.genAttrs (builtins.attrNames hosts) (hostName: fn hostName (builtins.getAttr hostName hosts));
in
{
  mkHosts = mkMkHosts mkHost;
  mkGuests = mkMkHosts mkGuest;
}
