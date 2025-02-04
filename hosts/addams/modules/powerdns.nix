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
  localAddress = lib.my.cidrToIp cfg.vlan.local.cidr;

  homeZone = config.repo.secrets.global.domain.home;
  workZone = config.repo.secrets.global.domain.work;

in
{
  services.resolved.enable = false;
  networking.resolvconf.useLocalResolver = true;

  services.powerdns = {
    enable = true;
    extraConfig = ''
      local-address=127.0.0.1:8853
      launch=gsqlite3
      gsqlite3-database=${directory}/pdns.sqlite3
      dnsupdate=yes
      primary=yes
      default-soa-content=@ hostmaster.@ 0 10800 3600 604800 3600
      enable-lua-records=yes
      webserver=no
      webserver-address=0.0.0.0
      webserver-allow-from=0.0.0.0/0
      api=no
      api-key=$API_KEY_HASH
      log-dns-queries=no
      log-dns-details=no
      loglevel=7
      query-logging=no
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
    # powerdns doesn't create the sqlite database for us
    # so we gotta either do it manually one-off or do the below to ensure it's created
    # if the file is missing before service start
    ExecStartPre = lib.mkBefore [
      (pkgs.writeScript "pdns-sqlite-init.sh" ''
        #!${pkgs.bash}/bin/bash

        pdns_folder="${directory}"
        echo "INIT: checking if pdns sqlite exists"
        if [ ! -f "${directory}/pdns.sqlite3" ]; then
          echo "INIT: no sqlite db found, initializing from pdns pkgs schema..."
          ${pkgs.sqlite}/bin/sqlite3 "${directory}/pdns.sqlite3" < "${pkgs.pdns}/share/doc/pdns/schema.sqlite3.sql"
          ${pkgs.busybox}/bin/chown pdns:pdns ${directory}/pdns.sqlite3
        fi

        # exit successfully
        exit 0
      '')
    ];
    ExecStartPost = (
      pkgs.writeScript "pdns-ddns-setup.sh" ''
        #!${pkgs.bash}/bin/bash

        cmd=${pkgs.pdns}/bin/pdnsutil

        $cmd create-zone home.arpa. || true
        $cmd create-zone ${homeZone}. || true
        $cmd create-zone ${workZone}. || true

        $cmd import-tsig-key kea hmac-sha512 $KEA_TSIG_KEY
        $cmd set-meta ${homeZone}. TSIG-ALLOW-DNSUPDATE kea
        $cmd set-meta ${workZone}. TSIG-ALLOW-DNSUPDATE kea
        ${builtins.concatStringsSep "\n" (
          map (zone: ''
            $cmd create-zone "${zone}" || true
            $cmd set-meta "${zone}" TSIG-ALLOW-DNSUPDATE kea
          '') cfg.reverseZones
        )}
      ''
    );
  };
}
