{
  description = "Ramblurr's packages, consumable without the full nixcfg flake";

  inputs = {
    nixpkgs.url = "github:ramblurr/nixpkgs/channel/personal-unstable";
    nixpkgs-mine.url = "git+https://github.com/ramblurr/nixpkgs?shallow=1&ref=channel/personal-master";
    clojure-nix-locker.url = "github:bevuta/clojure-nix-locker";
    clojure-nix-locker.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      overlays = import ./default.nix inputs;
      names = builtins.concatMap (overlay: builtins.attrNames (overlay { } { })) overlays;
      forSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;

      packages = forSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            config.allowUnfree = true;
          };
        in
        nixpkgs.lib.getAttrs names pkgs
      );
    };
}
