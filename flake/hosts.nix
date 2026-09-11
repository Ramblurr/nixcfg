{ self, inputs, ... }:
{
  flake =
    { config, lib, ... }:
    let
      hostHelpers = import ./nixos.nix {
        inherit
          self
          inputs
          lib
          config
          ;
      };
      mkDnsTerranix = import ../terranix/dns/package.nix;
      mkGarageTerranix = import ../terranix/garage/package.nix;
      inherit (hostHelpers)
        mkHost
        mkGuest
        mkHosts
        mkGuests
        ;

      hostInventory = import ../hosts/inventory.nix;
      # Executable additions stay out of the public, JSON-serialisable inventory.
      hostExtras = {
        debord = {
          hostExtraModules = [
            inputs.nad-api.nixosModules.default
            inputs.nixbot.nixosModules.nixbot
          ];
          hostOverlays = [ inputs.nad-api.overlays.default ];
        };
        addams.hostExtraModules = [
          inputs.nixos-nftables-firewall.nixosModules.default
        ];
        quine = {
          hostOverlays = [ ];
          hostExtraModules = [
            inputs.automatic-ripping-machine.nixosModules.default
            inputs.paseo.nixosModules.default
          ];
        };
        octoprint.hostOverlays = [ (import ../overlays/rpi4.nix) ];
        wyoming-satellite-bedroom.hostOverlays = [ (import ../overlays/rpi4.nix) ];
      };
      hosts =
        assert lib.assertMsg (lib.all (name: builtins.hasAttr name hostInventory) (
          builtins.attrNames hostExtras
        )) "Host additions must have a public inventory entry";
        lib.mapAttrs (
          name: host:
          (hostExtras.${name} or { })
          // {
            inherit (host) system isRpi;
            isStable =
              {
                stable = true;
                unstable = false;
              }
              .${host.channel};
          }
        ) hostInventory;
      guestNames = builtins.attrNames (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../guests)
      );
      guests = (lib.genAttrs guestNames (_name: { })) // {
        # set guest overrides
        # hello-world = { system ...};
      };
      guestHosts = {
        immich-home = "dewey";
        opencloud-home = "dewey";
      };
    in
    {
      lib.nixcfg = {
        inherit
          hosts
          hostInventory
          guests
          guestHosts
          mkHost
          mkGuest
          mkHosts
          mkGuests
          mkDnsTerranix
          mkGarageTerranix
          ;
      };

      # Do not export public nixosConfigurations from this flake.
      # The host modules intentionally depend on evaluation-time repo.secretFiles
      # supplied by ramblurr/nixcfg-private.  Keeping the concrete NixOS systems out of
      # the public flake output lets `nix flake show` inspect this flake without
      # trying to evaluate missing private site/global secrets.  Consumers that
      # have those secrets should build hosts via lib.nixcfg.mkHost/mkHosts; see
      # ramblurr/nixcfg-private/flake.nix's mkPrivateHost wrapper.
      nixosConfigurations = { };
    };
}
