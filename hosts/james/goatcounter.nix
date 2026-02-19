{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) work;

  domain = work;
  goatDomain = "goat.${domain}";

  goatcounterUser = "goatcounter";
  goatcounterGroup = "goatcounter";
  goatcounterStateDir = "/var/lib/goatcounter";
  goatcounterPort = 9100;

  # Keep SQLite tuning defaults explicit so DB behavior is stable over upgrades.
  goatcounterDb = "sqlite+${goatcounterStateDir}/db.sqlite3?_journal_mode=wal&_busy_timeout=200&_cache_size=-20000";

  goatcounterImportApiKeyFile = "${goatcounterStateDir}/import-api-key";
  goatcounterImportBackfillMarker = "${goatcounterStateDir}/import-backfill.done";
  nginxAccessLog = "/var/log/nginx/access.log";

  # Parse the JSON nginx log format defined in hosts/james/web.nix.
  goatcounterImportFormat = ''log:{"time": "$datetime","remote_addr": "$ignore","x_forwarded_for": "$xff","remote_user": "$ignore","bytes_sent": $ignore,"request_time": $ignore,"status": $status,"vhost": "$ignore","request_proto": "$http","path": "$path","request_query": "$query","request_length": $ignore,"duration": $ignore,"method": "$method","http_referrer": "$referrer","http_user_agent": "$user_agent","upstream_addr": "$ignore"}'';
  goatcounterImportFormatShell =
    lib.replaceStrings [ "$" "\"" ] [ "\\$" "\\\"" ]
      goatcounterImportFormat;
  goatcounterImportDatetime = "2006-01-02T15:04:05-07:00";

  goatcounterImportScript = pkgs.writeShellApplication {
    name = "goatcounter-import-nginx";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.goatcounter
      pkgs.gnused
      pkgs.gzip
    ];
    text = ''
      set -euo pipefail

      while [ ! -s '${goatcounterImportApiKeyFile}' ]; do
        echo "goatcounter-import: waiting for API key at ${goatcounterImportApiKeyFile}" >&2
        sleep 60
      done

      GOATCOUNTER_API_KEY="$(tr -d '\r\n' < '${goatcounterImportApiKeyFile}')"
      export GOATCOUNTER_API_KEY

      if [ ! -f '${goatcounterImportBackfillMarker}' ]; then
        echo "goatcounter-import: running one-time backfill from rotated nginx logs" >&2

        {
          find /var/log/nginx -maxdepth 1 -type f \
            \( -name 'access.log.[0-9]*' -o -name 'access.log.[0-9]*.gz' \) \
            | sort -V -r \
            | while IFS= read -r file; do
                case "$file" in
                  *.gz) gzip -dc -- "$file" ;;
                  *) cat -- "$file" ;;
                esac
              done

          if [ -f '${nginxAccessLog}' ]; then
            cat -- '${nginxAccessLog}'
          fi
        } | goatcounter import \
          -site 'https://${goatDomain}' \
          -format "${goatcounterImportFormatShell}" \
          -datetime '${goatcounterImportDatetime}' \
          -

        touch '${goatcounterImportBackfillMarker}'
      fi

      exec goatcounter import \
        -site 'https://${goatDomain}' \
        -follow \
        -format "${goatcounterImportFormatShell}" \
        -datetime '${goatcounterImportDatetime}' \
        '${nginxAccessLog}'
    '';
  };
in
{
  users.groups.${goatcounterGroup} = { };
  users.users.${goatcounterUser} = {
    isSystemUser = true;
    group = goatcounterGroup;
    home = goatcounterStateDir;
  };

  environment.persistence."/persist".directories = [
    {
      directory = goatcounterStateDir;
      user = goatcounterUser;
      group = goatcounterGroup;
      mode = "0750";
    }
  ];

  systemd.tmpfiles.rules = [
    "d ${goatcounterStateDir} 0750 ${goatcounterUser} ${goatcounterGroup} - -"
    "Z ${goatcounterStateDir} 0750 ${goatcounterUser} ${goatcounterGroup} - -"
    "f ${goatcounterImportApiKeyFile} 0640 ${goatcounterUser} ${goatcounterGroup} - -"
    "z ${goatcounterImportApiKeyFile} 0640 ${goatcounterUser} ${goatcounterGroup} - -"
  ];

  # Ensure local imports can resolve goat.${domain} to this host without hairpin routing.
  networking.hosts."127.0.0.1" = [ goatDomain ];

  systemd.services.goatcounter = {
    description = "GoatCounter analytics server";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = goatcounterUser;
      Group = goatcounterGroup;
      WorkingDirectory = goatcounterStateDir;
      ExecStart = lib.escapeShellArgs [
        "${pkgs.goatcounter}/bin/goatcounter"
        "serve"
        "-listen"
        "127.0.0.1:${toString goatcounterPort}"
        "-tls"
        "proxy"
        "-automigrate"
        "-db"
        goatcounterDb
      ];
      Restart = "always";
      RestartSec = "5s";
    };
  };

  systemd.services.goatcounter-import = {
    description = "Import nginx access logs into GoatCounter";
    after = [
      "goatcounter.service"
      "network-online.target"
      "nginx.service"
    ];
    wants = [
      "goatcounter.service"
      "network-online.target"
    ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = goatcounterUser;
      Group = goatcounterGroup;
      SupplementaryGroups = [ config.services.nginx.group ];
      WorkingDirectory = goatcounterStateDir;
      ExecStart = lib.getExe goatcounterImportScript;
      Restart = "always";
      RestartSec = "10s";
    };
  };

  security.acme.certs.${goatDomain}.domain = goatDomain;
  services.nginx.virtualHosts."${goatDomain}" = {
    useACMEHost = goatDomain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    extraConfig = ''
      allow 100.64.0.0/10;
      deny all;
    '';
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString goatcounterPort}";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $host;
      '';
    };
  };
}
