{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.repo.secrets) global;
  inherit (config.repo.secrets) home-ops;

  guests = [ "linkding" ];

  genGuestSecret = hostname: {
    "microvm-${hostname}-sops-key" = {
      sopsFile = ../../guests/${hostname}/secrets.sops.yaml;
      key = "ssh_host_ed25519_key";
      owner = "microvm";
      mode = "400";
    };
  };
  svcIp = hostname: lib.elemAt config.site.net.svc.hosts4.${hostname} 0;
  mkGuest =
    {
      hostname,
      domain,
      pgEnable ? false,
      pgUsername ? hostname,
      pgDatabase ? hostname,
      ip ? svcIp hostname,
    }:
    {
      modules.services.ingress.virtualHosts."${domain}.${global.domain.home}" = {
        acmeHost = global.domain.home;
        upstream = "http://${ip}:8080";
        forwardAuth = true;
      };
      modules.services.postgresql.extraAuthentication = lib.mkIf pgEnable [
        "host    ${pgUsername}    ${pgDatabase}    ${ip}/32    scram-sha-256"
      ];
    };
in
lib.mkMerge [
  {
    sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;
  }
  (mkGuest {
    hostname = "linkding";
    domain = "bookmarks";
    pgEnable = true;
  })
]
