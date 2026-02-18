{ inputs, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    let
      # Import our overlays once
      ourOverlays = import ../pkgs/default.nix inputs;

      # Extract package names from all overlays
      getOverlayPackages =
        overlay:
        let
          # Call the overlay with dummy arguments to extract attribute names
          # Since the overlay just returns an attrset of package definitions,
          # we can get the keys without actually building anything
          dummyPrev = { };
          overlayAttrs = overlay { } dummyPrev;
        in
        builtins.attrNames overlayAttrs;

      # Get all package names from all our overlays
      ourPackageNames = builtins.concatLists (builtins.map getOverlayPackages ourOverlays);
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = ourOverlays ++ [ inputs.nixos-extra-modules.overlays.default ];
      };

      # Export packages for flake consumers
      packages =
        # Export each package that exists in pkgs
        builtins.listToAttrs (
          builtins.filter (item: item != null) (
            builtins.map (
              name:
              if pkgs ? ${name} then
                {
                  inherit name;
                  value = pkgs.${name};
                }
              else
                null
            ) ourPackageNames
          )
        );

      # Optionally, also export as legacyPackages if you want to expose
      # the entire modified pkgs set (useful for consumers who want access
      # to all packages with your overlays applied)
      legacyPackages = pkgs;

    };
}
