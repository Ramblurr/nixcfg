{
  options,
  config,
  lib,
  pkgs,
  inputs,
  unstable,
  ...
}:
let
  cfg = config.modules.services.atuin-sync;
  home-ops = config.repo.secrets.home-ops;
  httpPort = toString cfg.ports.http;
  stateDirActual = "/var/lib/private/actual-budget";
  stateDirEffective = "/var/lib/actual-budget";
  cfgFile = pkgs.writeText "actual.json" (
    builtins.toJSON {
      dataDir = stateDirEffective;
      hostname = "127.0.0.1";
      port = cfg.ports.http;
      serverFiles = "${stateDirEffective}/server";
      userFiles = "${stateDirEffective}/user";
      loginMethod = "password";
      trustedProxies = [ "127.0.0.1/24" ];
    }
  );
in
{
  options.modules.services.atuin-sync = {
    enable = lib.mkEnableOption "atuin-sync";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "atuin.example.com";
      description = "The domain to use for the atuin-sync";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for the atuin-sync";
      };
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    services.atuin = {
      enable = true;
      openRegistration = false;
      port = cfg.ports.http;
    };
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      forwardAuth = false;
    };
  };
}
