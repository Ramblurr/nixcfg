{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkDefault removeSuffix;
  inherit (lib.my) mapModules;

  # nixosSystem: the flake system builder, accessed from the nixpkgs flake using nixpkgs.lib.nixosSystem
  mkHost =
    path:
    {
      nixosSystem,
      unstable,
      system,
      home-manager,
      overlays,
      extraModules,
      isStable,
      mine,
    }:
    nixosSystem {
      inherit system;
      specialArgs = {
        inherit lib inputs;
        unstable = unstable;
        mine = mine;
        # this provides modules a way to get a handle on what would normally be `inputs.nixpkgs`
        # but in a way that will ensure they get the input that corresponds to their stable/unstable version.
        actual-nixpkgs = if isStable then inputs.nixpkgs-stable else inputs.nixpkgs-unstable;
      };
      modules = extraModules ++ [
        { node.secretsDir = /.${path}/secrets; }
        {
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
          nixpkgs.overlays = overlays;
        }
        home-manager.nixosModules.home-manager
        inputs.quadlet-nix.nixosModules.quadlet
        (import path)
        ../. # /default.nix
      ];
    };
in
{
  mapHosts = dir: attrs: mapModules dir (hostPath: mkHost hostPath attrs);
}
