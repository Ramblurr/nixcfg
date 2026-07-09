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
      inherit (hostHelpers)
        mkHost
        mkGuest
        mkHosts
        mkGuests
        ;

      hosts = {
        debord = {
          # Guy Debord - https://en.wikipedia.org/wiki/Guy_Debord
          isStable = false;
          system = "x86_64-linux";
          hostExtraModules = [
            inputs.nad-api.nixosModules.default
            inputs.nixbot.nixosModules.nixbot
          ];
          # inputs.nixos-nftables-firewall.nixosModules.default
          hostOverlays = [ inputs.nad-api.overlays.default ];
        };
        addams = {
          # Jane Addams - https://en.wikipedia.org/wiki/Jane_Addams
          isStable = false;
          system = "x86_64-linux";
          hostExtraModules = [
            inputs.nixos-nftables-firewall.nixosModules.default
          ];
        };
        dewey = {
          # John Dewey - https://en.wikipedia.org/wiki/John_Dewey
          isStable = false;
          system = "x86_64-linux";
        };
        james = {
          # William James https://en.wikipedia.org/wiki/William_James
          isStable = false;
          system = "x86_64-linux";
        };
        witt = {
          # Ludwig Wittgenstein - https://en.wikipedia.org/wiki/Ludwig_Wittgenstein
          isStable = false;
          system = "x86_64-linux";
        };
        quine = {
          # Willard Van Orman Quine - https://en.wikipedia.org/wiki/Willard_Van_Orman_Quine
          isStable = false;
          system = "x86_64-linux";
          hostOverlays = [
          ];
          hostExtraModules = [
            inputs.automatic-ripping-machine.nixosModules.default
          ];
        };
        mali = {
          isStable = true;
          system = "x86_64-linux";
        };
        #rpi4 = {
        #  isStable = true;
        #  system = "aarch64-linux";
        #};
        octoprint = {
          isStable = true;
          isRpi = true;
          system = "aarch64-linux";
          hostOverlays = [
            (import ../overlays/rpi4.nix)
          ];
        };
        wyoming-satellite-bedroom = {
          isStable = true;
          isRpi = true;
          system = "aarch64-linux";
          hostOverlays = [
            (import ../overlays/rpi4.nix)
          ];
        };
        #_hello-world = {
        #  hostPath = ../guests/hello-world;
        #  enableDefaultModules = false;
        #  hostExtraModules = [
        #    inputs.impermanence.nixosModules.impermanence
        #    inputs.microvm.nixosModules.microvm
        #    inputs.sops-nix.nixosModules.sops
        #    ../config
        #    ../config/common-server.nix
        #    ../modules/microvm-guest
        #    ../modules/site
        #    ../modules/services/sshd.nix
        #    ../modules/secrets.nix
        #    ../modules/meta.nix
        #    ../modules/globals.nix
        #    ../modules/impermanence/default.nix
        #    ../guests/hello-world
        #  ];
        #};
      };
      guestNames = builtins.attrNames (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../guests)
      );
      guests = (lib.genAttrs guestNames (_name: { })) // {
        # set guest overrides
        # hello-world = { system ...};
      };
    in
    {
      lib.nixcfg = {
        inherit
          hosts
          guests
          mkHost
          mkGuest
          mkHosts
          mkGuests
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
