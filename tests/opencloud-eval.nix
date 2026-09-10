{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  configuration = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.quadlet-nix2.nixosModules.default
      ../modules/services/opencloud.nix
      {
        boot.isContainer = true;
        system.stateVersion = "26.05";
        modules.services.opencloud.instances = {
          alpha = {
            uid = 3101;
            gid = 3101;
            dataMount = "/mnt/alpha";
            domain = "alpha.example.test";
            office.domain = "docs-alpha.example.test";
            ports = {
              app = 9200;
              office = 9201;
            };
            environmentFile = "/run/credentials/alpha.env";
            office.environmentFile = "/run/credentials/alpha-office.env";
            oidc.issuer = "https://id.example.test";
          };
          beta = {
            uid = 3102;
            gid = 3102;
            dataMount = "/mnt/beta";
            domain = "beta.example.test";
            office.domain = "docs-beta.example.test";
            ports = {
              app = 9300;
              office = 9301;
            };
            environmentFile = "/run/credentials/beta.env";
            office.environmentFile = "/run/credentials/beta-office.env";
            oidc.issuer = "https://id.example.test";
          };
        };
      }
    ];
  };
  rejects =
    extra:
    let
      changed = configuration.extendModules { modules = [ extra ]; };
    in
    lib.any (a: !a.assertion) changed.config.assertions;
  c = configuration.config;
in
assert lib.all (a: a.assertion) c.assertions;
assert c.users.users.opencloud-alpha.uid == 3101;
assert c.users.users.opencloud-beta.uid == 3102;
assert c.users.users.opencloud-alpha.linger;
assert c.virtualisation.quadlet.containers.opencloud-alpha.uid == 3101;
assert c.virtualisation.quadlet.containers.opencloud-beta.uid == 3102;
assert
  c.virtualisation.quadlet.containers.opencloud-alpha.containerConfig.UserNS
  == "keep-id:uid=1000,gid=1000";
assert rejects { modules.services.opencloud.instances.beta.uid = lib.mkForce 3101; };
assert rejects { modules.services.opencloud.instances.beta.ports.app = lib.mkForce 9200; };
assert rejects {
  modules.services.opencloud.instances.beta.dataMount = lib.mkForce "/mnt/alpha/child";
};
assert rejects {
  modules.services.opencloud.instances.beta.stateDir =
    lib.mkForce "/var/lib/other/../opencloud-alpha";
};
pkgs.runCommand "opencloud-evaluation" { } ''
  touch $out
''
