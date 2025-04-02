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
    "linkding"
    "invoiceninja"
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
  (mkGuest {
    hostname = "linkding";
    domain = "bookmarks.${global.domain.home}";
    forwardAuth = true;
    pgEnable = true;
  })
  (mkGuest {
    hostname = "invoiceninja";
    domain = "clients2.${global.domain.work}";
    acmeHost = global.domain.work;
    dataset = "invoiceninja2";
    datasetMountpoint = "/var/lib/invoiceninja2";
  })
]
