{
  config,
  lib,
  pkgs,
  ...
}:
let
  directory = "/var/lib/pdns";
  user = "pdns";
  group = "pdns";
  cfg = config.repo.secrets.local;
  localAddress = lib.mori.first config.site.net.lan0.hosts4.addams;
  homeZone = config.repo.secrets.global.domain.home;
  workZone = config.repo.secrets.global.domain.work;
  pdns = pkgs.pdns-unstable;
in
{
  services.resolved.enable = false;
  networking.resolvconf.useLocalResolver = true;

  modules.services.powerdns = {
    enable = true;
    package = pdns;
    extraConfig = ''
      local-address=127.0.0.1:8853
      #launch=gsqlite3
      #gsqlite3-database=${directory}/pdns.sqlite3
      launch=lmdb
      lmdb-filename=${directory}/db.lmdb
      dnsupdate=yes
      primary=yes
      default-soa-content=@ hostmaster.@ 0 10800 3600 604800 3600
      enable-lua-records=yes
      webserver=yes
      webserver-port = 8068
      webserver-address=127.0.0.1
      webserver-allow-from=0.0.0.0/0
      api=yes
      api-key=$API_KEY_HASH
      log-dns-queries=no
      log-dns-details=no
      query-logging=no
      loglevel=7
      views=yes
      proxy-protocol-from=127.0.0.1
    '';
    secretFile = config.sops.secrets."powerdns/env".path;
  };

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/powerdns"."mountpoint" = directory;
    "rpool/encrypted/safe/svc/powerdns"."com.sun:auto-snapshot" = "false";
  };
  systemd.tmpfiles.rules = [ "d ${directory} 0750 ${user} ${group}" ];

  sops.secrets."powerdns/env" = {
    owner = user;
    group = group;
  };

  systemd.services.pdns.serviceConfig = {
    ExecStartPost = (
      pkgs.writeScript "pdns-ddns-setup.sh" ''
        #!${pkgs.bash}/bin/bash

        cmd=${pdns}/bin/pdnsutil

        $cmd zone create home.arpa. || true
        $cmd zone create ${homeZone}. || true
        $cmd zone create ${workZone}. || true

        $cmd tsigkey import kea hmac-sha512 $KEA_TSIG_KEY
        $cmd metadata set ${homeZone}. TSIG-ALLOW-DNSUPDATE kea
        $cmd metadata set ${workZone}. TSIG-ALLOW-DNSUPDATE kea
        ${builtins.concatStringsSep "\n" (
          map (zone: ''
            $cmd zone create "${zone}" || true
            $cmd metadata set "${zone}" TSIG-ALLOW-DNSUPDATE kea
          '') cfg.reverseZones
        )}
      ''
    );
  };
}
