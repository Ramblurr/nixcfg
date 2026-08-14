{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  homeDomain = "so" + "cozy.casa";
  workDomain = "outskirtslabs.com";
  sopsOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption { type = lib.types.attrs; };
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
        sops.secrets = lib.mkOption {
          type = lib.types.attrsOf (
            lib.types.submodule (
              { name, ... }:
              {
                options = {
                  path = lib.mkOption {
                    type = lib.types.str;
                    default = "/run/secrets/${name}";
                  };
                  restartUnits = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                  };
                };
              }
            )
          );
          default = { };
        };
      };
    };
  evaluated = lib.nixosSystem {
    modules = [
      ../modules/services/pocket-id.nix
      ../hosts/james/pocket-id.nix
      sopsOptions
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "24.11";
        services.nginx.enable = true;
        security.acme = {
          acceptTerms = true;
          defaults.email = "admin@example.test";
        };
        repo.secrets.global.domain = {
          home = homeDomain;
          work = workDomain;
        };
      }
    ];
  };
  cfg = evaluated.config;
  home = cfg.containers.pocket-id-home;
  work = cfg.containers.pocket-id-work;
  homeService = cfg.systemd.services."container@pocket-id-home";
  workService = cfg.systemd.services."container@pocket-id-work";
  homePocketId = home.config.services.pocket-id;
  workPocketId = work.config.services.pocket-id;
  homeDataPath = "/var/lib/pocket-id-containers/id.${homeDomain}";
  workDataPath = "/var/lib/pocket-id-containers/id.${workDomain}";
  secretPath = "/run/secrets/pocket-id-encryption-key";
in
assert
  builtins.attrNames cfg.containers == [
    "pocket-id-home"
    "pocket-id-work"
  ];
assert home.autoStart && work.autoStart;
assert !home.privateNetwork && !work.privateNetwork;
assert
  home.bindMounts."/var/lib/pocket-id" == {
    hostPath = homeDataPath;
    isReadOnly = false;
    mountPoint = "/var/lib/pocket-id";
  };
assert
  work.bindMounts."/var/lib/pocket-id" == {
    hostPath = workDataPath;
    isReadOnly = false;
    mountPoint = "/var/lib/pocket-id";
  };
assert home.bindMounts.${secretPath}.hostPath == "/run/secrets/home-pocket-id-encryption-key";
assert work.bindMounts.${secretPath}.hostPath == "/run/secrets/work-pocket-id-encryption-key";
assert home.bindMounts.${secretPath}.isReadOnly && work.bindMounts.${secretPath}.isReadOnly;
assert
  cfg.modules.zfs.datasets.properties."rpool/encrypted/safe/svc/id.${homeDomain}".mountpoint
  == homeDataPath;
assert
  cfg.modules.zfs.datasets.properties."rpool/encrypted/safe/svc/id.${workDomain}".mountpoint
  == workDataPath;
assert
  cfg.sops.secrets.home-pocket-id-encryption-key.restartUnits
  == [ "container@pocket-id-home.service" ];
assert
  cfg.sops.secrets.work-pocket-id-encryption-key.restartUnits
  == [ "container@pocket-id-work.service" ];
assert homePocketId.credentials.ENCRYPTION_KEY == secretPath;
assert workPocketId.credentials.ENCRYPTION_KEY == secretPath;
assert homePocketId.settings.APP_URL == "https://id.${homeDomain}";
assert homePocketId.settings.HOST == "127.0.0.1";
assert homePocketId.settings.PORT == 1411;
assert homePocketId.settings.TRUST_PROXY;
assert workPocketId.settings.APP_URL == "https://id.${workDomain}";
assert workPocketId.settings.HOST == "127.0.0.1";
assert workPocketId.settings.PORT == 1412;
assert workPocketId.settings.TRUST_PROXY;
assert home.config.users.users.pocket-id.uid == 988;
assert home.config.users.groups.pocket-id.gid == 987;
assert work.config.users.users.pocket-id.uid == 988;
assert work.config.users.groups.pocket-id.gid == 987;
assert !(builtins.hasAttr "pocket-id" cfg.systemd.services);
assert builtins.elem "sops-install-secrets.service" homeService.requires;
assert builtins.elem "zfs-datasets.service" homeService.requires;
assert builtins.elem "sops-install-secrets.service" homeService.after;
assert builtins.elem "zfs-datasets.service" homeService.after;
assert builtins.elem homeDataPath homeService.unitConfig.RequiresMountsFor;
assert builtins.elem "/run/secrets/home-pocket-id-encryption-key"
  homeService.unitConfig.RequiresMountsFor;
assert builtins.elem workDataPath workService.unitConfig.RequiresMountsFor;
assert builtins.elem "/run/secrets/work-pocket-id-encryption-key"
  workService.unitConfig.RequiresMountsFor;
assert cfg.security.acme.certs."id.${homeDomain}".domain == "id.${homeDomain}";
assert cfg.security.acme.certs."id.${workDomain}".domain == "id.${workDomain}";
assert
  cfg.services.nginx.virtualHosts."id.${homeDomain}".locations."/".proxyPass
  == "http://127.0.0.1:1411";
assert
  cfg.services.nginx.virtualHosts."id.${workDomain}".locations."/".proxyPass
  == "http://127.0.0.1:1412";
pkgs.runCommand "james-pocket-id-module-test" { } ''
  touch "$out"
''
