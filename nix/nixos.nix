{
  inputs,
  self,
  lib,
  ...
}:

let

  defaultModules = [
    (import ../modules/default.nix)
    inputs.quadlet-nix.nixosModules.quadlet
    inputs.impermanence.nixosModules.impermanence
    inputs.sops-nix.nixosModules.sops
  ];
  unstableDefaultModules = [
    inputs.disko-unstable.nixosModules.disko
    (import ../modules-unstable/default.nix)
  ];
  stableDefaultModules = [ inputs.disko-stable.nixosModules.disko ];

  defaultOverlays = [
    inputs.nixos-extra-modules.overlays.default
    inputs.emacs-overlay.overlays.default
    (import ../lib inputs)
    (import ../overlays/last-known-good.nix)
    (import ../overlays/roon-server.nix)
    (import ../overlays/logseq.nix)
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
      nixpkgs-mine = mkPkgs {
        inherit system;
        overlays = allOverlays;
        flake = inputs.nixpkgs-mine;
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
            networking.hostName = lib.mkForce name;
            node.secretsDir = hostPath + "/secrets";
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
        mine = nixpkgs-mine;
        actual-nixpkgs = actual-nixpkgs;
        unstable = nixpkgs-unstable;
        lib = nixpkgs'.lib;
        hostName = name;
      };
    };

in
{
  inherit mkPkgs;

  mkHosts =
    hosts:
    lib.genAttrs (builtins.attrNames hosts) (
      hostName: mkHost hostName (builtins.getAttr hostName hosts)
    );
}
