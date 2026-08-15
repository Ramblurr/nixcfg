{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;

  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption { type = lib.types.attrs; };
        modules.networking.systemd-netns-private = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        modules.services.ingress = {
          domains = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          virtualHosts = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      };
    };

  evaluated = lib.nixosSystem {
    specialArgs = { inherit inputs; };
    modules = [
      ../modules/services/ocis.nix
      testOptions
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        repo.secrets = {
          global.nodes.mali.dataCIDR = "192.0.2.1/24";
          home-ops.mail = {
            host = "192.0.2.2";
            port = 25;
            notificationsFromAddressWork = "notifications@example.test";
          };
        };
        modules.services.ocis = {
          enable = true;
          domain = "data.example.test";
          cspYaml = "---\ndirectives: {}\n";
          ports.http = 9200;
          nfsShare = "ocis";
          user = {
            name = "ocis";
            uid = 991;
          };
          group = {
            name = "ocis";
            gid = 991;
          };
          subnet = {
            hostAddr = "192.0.2.1/30";
            nsAddr = "192.0.2.2/30";
          };
          ingress = {
            domain = "example.test";
            external = true;
          };
          oidc = {
            issuer = "https://id.example.test";
            clientId = "ocis-web";
            scopes = [
              "openid"
              "profile"
              "email"
              "groups"
            ];
            autoProvisionAccounts = false;
            userOidcClaim = "preferred_username";
            userCs3Claim = "username";
            roleAssignmentDriver = "default";
            rewriteWellKnown = true;
            accessTokenVerifyMethod = "jwt";
          };
        };
      }
    ];
  };

  workEvaluated = lib.evalModules {
    specialArgs = { inherit pkgs; };
    modules = [
      ../config/home-ops.nix
      (
        { lib, ... }:
        {
          options = {
            modules.services.ocis = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            modules.users.primaryUser.username = lib.mkOption { type = lib.types.str; };
            networking.hostName = lib.mkOption { type = lib.types.str; };
            repo.secrets = lib.mkOption { type = lib.types.attrs; };
          };
          config = {
            _module.check = false;
            networking.hostName = "dewey";
            modules.users.primaryUser.username = "test-user";
            repo.secrets = {
              global.nodes.dewey = { };
              home-ops = {
                workDomain = "work.example.test";
                ports.ocis-http = 9200;
                users.ocis-work = {
                  name = "ocis";
                  uid = 991;
                };
                groups.ocis-work = {
                  name = "ocis";
                  gid = 991;
                };
                ocis-work-csp = "---\ndirectives: {}\n";
                subnets.ocis-work = {
                  hostAddr = "192.0.2.1/30";
                  nsAddr = "192.0.2.2/30";
                };
              };
            };
            home-ops = {
              enable = true;
              apps.ocis-work.enable = true;
            };
          };
        }
      )
    ];
  };

  workOcis = workEvaluated.config.modules.services.ocis;
  environment = evaluated.config.services.ocis.environment;
in
assert environment.OCIS_OIDC_ISSUER == "https://id.example.test";
assert environment.WEB_OIDC_CLIENT_ID == "ocis-web";
assert environment.PROXY_OIDC_ACCESS_TOKEN_VERIFY_METHOD == "jwt";
assert environment.PROXY_OIDC_REWRITE_WELLKNOWN == "true";
assert environment.PROXY_AUTOPROVISION_ACCOUNTS == "false";
assert environment.PROXY_USER_OIDC_CLAIM == "preferred_username";
assert environment.PROXY_USER_CS3_CLAIM == "username";
assert environment.PROXY_ROLE_ASSIGNMENT_DRIVER == "default";
assert environment.WEB_OIDC_SCOPE == "openid profile email groups";
assert !lib.hasInfix "application/o" (builtins.toJSON environment);
assert workOcis.oidc.issuer == "https://id.work.example.test";
assert workOcis.oidc.clientId == "work-ocis";
assert
  workOcis.oidc.scopes == [
    "openid"
    "profile"
    "email"
    "groups"
  ];
assert !workOcis.oidc.autoProvisionAccounts;
assert workOcis.oidc.userOidcClaim == "preferred_username";
assert workOcis.oidc.userCs3Claim == "username";
assert workOcis.oidc.roleAssignmentDriver == "default";
assert workOcis.oidc.rewriteWellKnown;
assert workOcis.oidc.accessTokenVerifyMethod == "jwt";
assert !lib.hasInfix "application/o" (builtins.toJSON workOcis.oidc);
pkgs.runCommand "ocis-oidc-module-test" { } ''
  touch "$out"
''
