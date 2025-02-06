{ self, inputs, ... }:
{
  flake =
    { config, lib, ... }:
    let
      inherit (lib)
        concatMapAttrs
        filterAttrs
        flip
        genAttrs
        mapAttrs
        mapAttrs'
        nameValuePair
        ;
      mkHosts =
        (import ./nixos.nix {
          inherit
            self
            inputs
            lib
            config
            ;
        }).mkHosts;

      hosts = {
        debord = {
          # Guy Debord - https://en.wikipedia.org/wiki/Guy_Debord
          isStable = false;
          system = "x86_64-linux";
          #hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        };
        addams = {
          # Jane Addams - https://en.wikipedia.org/wiki/Jane_Addams
          isStable = false;
          system = "x86_64-linux";
          hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        };
        dewey = {
          # John Dewey - https://en.wikipedia.org/wiki/John_Dewey
          isStable = false;
          system = "x86_64-linux";
          hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
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
            (import ../overlays/qemu.nix)
            inputs.nix-writers.overlays.default
          ];
        };
        mali = {
          isStable = true;
          system = "x86_64-linux";
        };
      };
    in
    {
      nixosConfigurations = (mkHosts hosts) // {
        addams-installer = inputs.nixpkgs-unstable.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            targetSystem = inputs.self.nixosConfigurations.addams;
          };
          modules = [
            ../hosts/addams/installer.nix
          ];
        };

      };
      # True NixOS nodes can define additional guest nodes that are built
      # together with it. We collect all defined guests from each node here
      # to allow accessing any node via the unified attribute `nodes`.
      #guestConfigs = flip concatMapAttrs config.nixosConfigurations (
      #  _: node:
      #  flip mapAttrs' (node.config.guests or { }) (
      #    guestName: guestDef:
      #    nameValuePair guestDef.nodeName (
      #      if guestDef.backend == "microvm" then
      #        node.config.microvm.vms.${guestName}.config
      #      else
      #        node.config.containers.${guestName}.nixosConfiguration
      #    )
      #  )
      #);
      # All nixosSystem instanciations are collected here, so that we can refer
      # to any system via nodes.<name>

      nodes = config.nixosConfigurations; # // config.guestConfigs;
    };
}
