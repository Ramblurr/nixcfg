{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  addresses = {
    debord = "192.0.2.21";
    dewey = "192.0.2.14";
    mali = "192.0.2.3";
  };
  virtualAddress = "192.0.2.22";
  credentialsFile = pkgs.writeText "onepassword-connect-test-secrets.yaml" "{}\n";
  user = {
    name = "op";
    uid = 3007;
  };
  group = {
    name = "op";
    gid = 3007;
  };
  evaluate =
    name:
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      specialArgs = { inherit inputs; };
      modules = [
        (import ../modules/quadlet-nix.nix inputs)
        inputs.home-manager.nixosModules.home-manager
        inputs.sops-nix.nixosModules.sops
        ../modules/zfs-attrs.nix
        ../modules/services/onepassword-connect.nix
        {
          options = {
            modules.services.caddy = {
              routes = lib.mkOption {
                type = lib.types.attrs;
                default = { };
              };
              edge.certificateHosts = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };
            };
            repo.secrets = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            site = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
        }
        {
          networking = {
            hostName = name;
            nftables.enable = true;
          };
          system.stateVersion = "26.05";
          home-manager.users.${user.name}.home.stateVersion = "26.05";
          repo.secrets = {
            global.nameservers = [ "192.0.2.53" ];
            home-ops = {
              ports.onepassword-connect-sync = 10003;
              users.onepassword-connect = user;
              groups.onepassword-connect = group;
            };
          };
          site.net.mgmt.hosts4 = (lib.mapAttrs (_: address: [ address ]) addresses) // {
            onepassword-connect = [ virtualAddress ];
          };
          systemd.network.networks."30-mgmt".addresses = [
            { Address = "${addresses.${name} or "192.0.2.99"}/24"; }
          ];
          modules.services.onepassword-connect.credentialsFile = credentialsFile;
        }
      ];
    }).config;
in
{
  inherit
    addresses
    virtualAddress
    credentialsFile
    user
    group
    evaluate
    ;
}
