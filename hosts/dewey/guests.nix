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

  guests = [
  ];

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
      forwardAuth ? false,
      external ? false,
      acmeHost ? global.domain.home,
      pgEnable ? false,
      pgUsername ? hostname,
      pgDatabase ? hostname,
      ip ? svcIp hostname,
      dataset ? hostname,
      datasetMountpoint ? "/var/lib/${hostname}",
    }:
    {
      modules.services.ingress.virtualHosts."${domain}" = {
        acmeHost = acmeHost;
        upstream = "http://${ip}:8080";
        forwardAuth = forwardAuth;
      };
      modules.services.ingress.domains = lib.mkIf external {
        "${acmeHost}" = {
          externalDomains = [ domain ];
        };
      };
      modules.services.postgresql.extraAuthentication = lib.mkIf pgEnable [
        "host    ${pgUsername}    ${pgDatabase}    ${ip}/32    scram-sha-256"
      ];

      modules.zfs.datasets.properties = lib.mkIf (dataset != null && dataset != "") {
        "rpool/encrypted/safe/svc/${dataset}" = {
          "mountpoint" = datasetMountpoint;
          "com.sun:auto-snapshot" = "false";
        };
      };
    };
in
lib.mkMerge [
  {
    sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;
  }
]
