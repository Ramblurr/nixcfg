{ inputs, ... }:
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
      mkHosts = (import ./nixos.nix { inherit inputs lib config; }).mkHosts;

      hosts = {
        #debord = {
        #  isStable = false;
        #  system = "x86_64-linux";
        #  #hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        #};
        #dewey = {
        #  isStable = false;
        #  system = "x86_64-linux";
        #  hostExtraModules = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        #};
        #aquinas = {
        #  isStable = false;
        #  system = "x86_64-linux";
        #};
        witt = {
          isStable = false;
          system = "x86_64-linux";
        };
        quine = {
          isStable = false;
          system = "x86_64-linux";
          hostOverlays = [ (import ../overlays/qemu.nix) ];
        };
        mali = {
          isStable = true;
          system = "x86_64-linux";
        };
      };
    in
    {
      nixosConfigurations = mkHosts hosts;
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
