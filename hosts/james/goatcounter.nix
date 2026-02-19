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

  goatcounterImportApiKeysDir = "${goatcounterStateDir}/import-api-keys";
  goatcounterImportBackfillMarker = "${goatcounterStateDir}/import-backfill.done";
  nginxAccessLog = "/var/log/nginx/access.log";

  # Parse the JSON nginx log format defined in hosts/james/web.nix.
  goatcounterImportFormat = ''log:{"time": "$datetime","remote_addr": "$ignore","x_forwarded_for": "$xff","remote_user": "$ignore","bytes_sent": $ignore,"request_time": $ignore,"status": $status,"vhost": "$host","request_proto": "$http","path": "$path","request_query": "$ignore","request_length": $ignore,"duration": $ignore,"method": "$method","http_referrer": "$referrer","http_user_agent": "$user_agent","upstream_addr": "$ignore"}'';
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

      list_key_files() {
        find '${goatcounterImportApiKeysDir}' -maxdepth 1 -type f -size +0c \
          | LC_ALL=C sort
      }

      # Escape regex metacharacters so hostnames are matched literally.
      escape_regex() {
        printf '%s' "$1" | sed -E 's/[][(){}.^$*+?|\\]/\\&/g'
      }

      run_import() {
        local site_host="$1"
        local key_file="$2"
        local source="$3"
        local extra_flag="''${4:-}"
        local site_host_re
        local api_key
        site_host_re="$(escape_regex "$site_host")"
        api_key="$(tr -d '\r\n' < "$key_file")"

        GOATCOUNTER_API_KEY="$api_key" goatcounter import \
          -site 'https://${goatDomain}' \
          -format "${goatcounterImportFormatShell}" \
          -datetime '${goatcounterImportDatetime}' \
          -exclude static \
          -exclude redirect \
          -exclude 'path:re:^$' \
          -exclude 'host:${goatDomain}' \
          -exclude 'host:goaccess-internal' \
          -exclude "!host:re:^''${site_host_re}$" \
          ${"$"}{extra_flag:+${"$"}extra_flag} \
          "$source"
      }

      while true; do
        mapfile -t key_files < <(list_key_files)
        if [ ''${#key_files[@]} -eq 0 ]; then
          echo "goatcounter-import: waiting for non-empty key files in ${goatcounterImportApiKeysDir}" >&2
          sleep 60
          continue
        fi

        if [ ! -f '${goatcounterImportBackfillMarker}' ]; then
          echo "goatcounter-import: running one-time backfill from rotated nginx logs for ''${#key_files[@]} vhost(s)" >&2

          for key_file in "''${key_files[@]}"; do
            site_host="$(basename "$key_file")"
            echo "goatcounter-import: backfilling $site_host" >&2

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
            } | run_import "$site_host" "$key_file" -
          done

          touch '${goatcounterImportBackfillMarker}'
        fi

        pids=()
        for key_file in "''${key_files[@]}"; do
          site_host="$(basename "$key_file")"
          echo "goatcounter-import: following ${nginxAccessLog} for $site_host" >&2
          (
            run_import "$site_host" "$key_file" '${nginxAccessLog}' "-follow"
          ) &
          pids+=("$!")
        done

        # If any follower exits, restart the full group to keep all importers aligned.
        wait -n "''${pids[@]}" || true
        for pid in "''${pids[@]}"; do
          kill "$pid" 2>/dev/null || true
        done
        wait || true
        sleep 2
      done
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
    "d ${goatcounterImportApiKeysDir} 0750 ${goatcounterUser} ${goatcounterGroup} - -"
    "z ${goatcounterImportApiKeysDir} 0750 ${goatcounterUser} ${goatcounterGroup} - -"
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
      allow 127.0.0.1;
      allow ::1;
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
