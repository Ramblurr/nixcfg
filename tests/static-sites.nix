{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  base = (import ./james-caddy.nix { inherit inputs pkgs; }).evaluated;
  site = {
    domain = "static.example.test";
    authorizedKeys = [ "ssh-ed25519 AAAATEST" ];
  };
  failures =
    patch:
    map (entry: entry.message) (
      lib.filter (entry: !entry.assertion) (base.extendModules { modules = [ patch ]; }).config.assertions
    );
  rejects = patch: message: lib.any (lib.hasInfix message) (failures patch);
  sameUser = {
    modules.services.staticSites.sites = {
      first = site // {
        path = "/first/";
        deployUser = "shared-deploy";
      };
      second = site // {
        path = "/second/";
        deployUser = "shared-deploy";
        enable = false;
      };
    };
  };
  sharedHost = {
    modules.services.staticSites.sites = {
      first = site // {
        path = "/first/";
      };
      second = site // {
        path = "/second/";
        deployUser = "custom-publisher";
      };
    };
  };
  disabled =
    (base.extendModules {
      modules = [
        {
          modules.services.staticSites.sites.epsilon.enable = false;
        }
      ];
    }).config;
in
assert failures sharedHost == [ ];
assert rejects sameUser "different deployment user";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    path = "/missing-slash";
  };
} "normalized URL mount";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    path = "/../escape/";
  };
} "normalized URL mount";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    domain = "*.example.test";
  };
} "exact lowercase DNS";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    deployUser = "root";
  };
} "reserved service accounts";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    authorizedKeys = [ ];
  };
} "public key";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    directory = "/var/lib/static-sites";
  };
} "directory beneath";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    directory = "/var/lib/static-web/stardustdb.guide";
  };
} "directories must not overlap";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    domain = "stardustdb.guide";
    path = "/dust/child/";
  };
} "URL mounts";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    domain = "data-star.guide";
  };
} "existing James route";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    domain = "books.home.example.test";
  };
} "shadow a Dewey";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    cacheControlByPath."/assets/*.js" = "no-cache";
  };
} "cache overrides";
assert rejects {
  modules.services.staticSites.sites.bad = site // {
    cacheControlByMimeType."*/javascript" = "no-cache";
  };
} "cache overrides";
assert disabled.modules.services.staticSites.handlersByDomain == { };
assert disabled.users.users.epsilon-deploy.openssh.authorizedKeys.keys == [ ];
assert lib.hasInfix (builtins.unsafeDiscardStringContext "ForceCommand ${pkgs.coreutils}/bin/false")
  disabled.services.openssh.extraConfig;
assert builtins.elem
  "d /var/lib/static-web/stardustdb.guide/epsilon 0755 epsilon-deploy epsilon-deploy - -"
  disabled.systemd.tmpfiles.rules;
pkgs.runCommand "static-sites-eval-test" { } "touch $out"
