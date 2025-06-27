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
  ] ++ (import ../pkgs/default.nix inputs);

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
          "olm-3.2.16"
        ];
      };
    };

  mkHost =
    name:
    {
      isStable ? false,
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
    in
    actual-nixpkgs.lib.nixosSystem {
      inherit system;
      modules =
        [
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
        actual-nixpkgs = actual-nixpkgs;
        unstable = nixpkgs-unstable;
        lib = nixpkgs'.lib;
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
        nixpkgs = inputs.nixpkgs;
      };
      modules = [
        inputs.quadlet-nix.nixosModules.quadlet
        ../config/guests.nix
        {
          node.secretsDir = hostPath + "/secrets";
          sops.defaultSopsFile = hostPath + "/secrets.sops.yaml";
          node.name = name;
          nixpkgs.config.allowUnfree = true;
          modules.microvm-guest.enable = true;
        }
        ../guests/${name}
      ] ++ extraModules;
    };

  mkMkHosts =
    fn: hosts:
    lib.genAttrs (builtins.attrNames hosts) (hostName: fn hostName (builtins.getAttr hostName hosts));
in
{
  mkHosts = mkMkHosts mkHost;
  mkGuests = mkMkHosts mkGuest;
}
