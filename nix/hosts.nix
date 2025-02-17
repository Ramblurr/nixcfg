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
      hostHelpers = (
        import ./nixos.nix {
          inherit
            self
            inputs
            lib
            config
            ;
        }
      );
      inherit (hostHelpers) mkHosts;

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
          hostExtraModules = [
            inputs.nixos-nftables-firewall.nixosModules.default
            inputs.crowdsec.nixosModules.crowdsec
            inputs.crowdsec.nixosModules.crowdsec-firewall-bouncer
          ];
          hostOverlays = [ inputs.crowdsec.overlays.default ];
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
            #inputs.nix-writers.overlays.default
          ];
        };
        mali = {
          isStable = true;
          system = "x86_64-linux";
        };
        hello-world = {
          hostPath = ../guests/hello-world;
          enableDefaultModules = false;
          hostExtraModules = [
            inputs.impermanence.nixosModules.impermanence
            inputs.microvm.nixosModules.microvm
            inputs.sops-nix.nixosModules.sops
            ../config/secrets.nix
            ../config/common-server.nix
            ../config/site.nix
            ../modules/site
            ../modules/services/sshd.nix
            ../modules/secrets.nix
            ../modules/meta.nix
            ../modules/globals.nix
            ../modules/impermanence/default.nix
            ../modules/microvm-guest
            ../guests/hello-world
          ];
        };
      };
    in
    {
      nixosConfigurations = (mkHosts hosts) // {
        addams-installer = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            targetSystem = inputs.self.nixosConfigurations.addams;
          };
          modules = [
            ../hosts/addams/installer.nix
          ];

        };
      };
      # All nixosSystem instanciations are collected here, so that we can refer
      # to any system via nodes.<name>
      nodes = config.nixosConfigurations;
    };
}
