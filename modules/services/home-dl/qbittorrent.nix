{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.home-dl;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  inherit (config.repo.secrets) home-ops;

  mediaUser = home-ops.users.media.name;
  mediaGroup = home-ops.groups.media.name;
  quiUser = home-ops.users.qui;
  quiGroup = home-ops.groups.qui;
  stateDir = "/var/lib/private/home-dl";
  downloadsDir = "/mnt/downloads";
  qbittorrentDownloadsDir = "${downloadsDir}/torrents/qbit";
  qbittorrentBlackholeDir = "${qbittorrentDownloadsDir}/blackhole";
  qbittorrentTorrentFilesDir = "${qbittorrentDownloadsDir}/torrents";
  qbittorrentFinishedTorrentFilesDir = "${qbittorrentDownloadsDir}/torrents-complete";
  qbittorrentStateDir = "${stateDir}/qbittorrent";
  qbittorrentProfileDir = "/var/lib/qbittorrent";
  quiStateDir = "${stateDir}/qui";
  qbittorrentDomain = "qbittorrent.${cfg.baseDomain}";

  namespace = "qbtvpn";
  namespaceAddress = "192.168.15.1";
  bridgeAddress = "192.168.15.5";
  wireguardInterface = "${namespace}0";
  protonGateway = "10.2.0.1";
  wireguardConfigFile = "/run/${namespace}/wireguard.conf";
  qbittorrentApiPort = 8085;

  qbittorrentWatchedFolders = pkgs.writeText "qbittorrent-watched-folders.json" (
    builtins.toJSON {
      ${qbittorrentBlackholeDir} = {
        add_torrent_params = {
          content_layout = "Subfolder";
          save_path = "${qbittorrentDownloadsDir}/complete";
          use_auto_tmm = false;
        };
        recursive = false;
      };
    }
  );
  portForwardScript = pkgs.writeShellScript "proton-qbittorrent-port-forward" ''
    set -u

    gateway=${lib.escapeShellArg protonGateway}
    wireguard_interface=${lib.escapeShellArg wireguardInterface}
    api_url=${lib.escapeShellArg "http://127.0.0.1:${toString qbittorrentApiPort}/api/v2/app/setPreferences"}
    firewall_chain=qbt-pf
    current_port=
    status_file="''${RUNTIME_DIRECTORY:-/run/proton-qbittorrent-port-forward}/port"

    log() {
      printf '%s\n' "proton-qbittorrent-port-forward: $*" >&2
    }

    parse_mapping() {
      output=$1
      expected_protocol=$2
      result_lines=$(printf '%s\n' "$output" | grep -E '^Mapped public port [0-9]+ protocol (UDP|TCP) to local port 0 lifetime [0-9]+$' || true)
      result_count=$(printf '%s\n' "$result_lines" | grep -c . || true)

      if [ "$result_count" -ne 1 ]; then
        log "expected exactly one successful NAT-PMP result for $expected_protocol"
        return 1
      fi

      # shellcheck disable=SC2086
      set -- $result_lines
      port=$4
      protocol=$6
      shift 9
      private_port=$1
      shift 2
      lifetime=$1

      case "$port" in
        ""|*[!0-9]*) log "NAT-PMP returned an invalid public port"; return 1 ;;
      esac
      if [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
        log "NAT-PMP returned an out-of-range public port"
        return 1
      fi
      if [ "$protocol" != "$expected_protocol" ] || [ "$private_port" != 0 ] || [ "$lifetime" != 60 ]; then
        log "NAT-PMP returned unexpected protocol, private port, or lifetime"
        return 1
      fi

      printf '%s\n' "$port"
    }

    map_port() {
      public_port=$1
      protocol=$2
      expected_protocol=$(printf '%s' "$protocol" | tr '[:lower:]' '[:upper:]')

      if ! output=$(natpmpc -g "$gateway" -a "$public_port" 0 "$protocol" 60 2>&1); then
        log "NAT-PMP $expected_protocol request failed: $output"
        return 1
      fi
      parse_mapping "$output" "$expected_protocol"
    }

    set_qbittorrent() {
      port=$1
      interface=$2
      preferences=$(printf '{"listen_port":%s,"current_network_interface":"%s","random_port":false,"upnp":false}' "$port" "$interface")
      curl --fail --silent --show-error --max-time 10 \
        --data-urlencode "json=$preferences" "$api_url" >/dev/null
    }

    close_firewall() {
      iptables -w -D INPUT -i "$wireguard_interface" -j "$firewall_chain" 2>/dev/null || true
      iptables -w -F "$firewall_chain" 2>/dev/null || true
      iptables -w -X "$firewall_chain" 2>/dev/null || true
    }

    open_firewall() {
      port=$1
      close_firewall
      iptables -w -N "$firewall_chain"
      iptables -w -A "$firewall_chain" -p tcp --dport "$port" -j ACCEPT
      iptables -w -A "$firewall_chain" -p udp --dport "$port" -j ACCEPT
      iptables -w -I INPUT 1 -i "$wireguard_interface" -j "$firewall_chain"
    }

    release_mapping() {
      port=$1
      natpmpc -g "$gateway" -a "$port" 0 udp 0 >/dev/null 2>&1 || true
      natpmpc -g "$gateway" -a "$port" 0 tcp 0 >/dev/null 2>&1 || true
    }

    cleanup() {
      close_firewall
      rm -f "$status_file"
      set_qbittorrent 0 lo >/dev/null 2>&1 || true
      if [ -n "$current_port" ]; then
        release_mapping "$current_port"
        current_port=
      fi
    }

    acquire_mapping() {
      udp_port=$(map_port 1 udp) || return 1
      if ! tcp_port=$(map_port "$udp_port" tcp); then
        release_mapping "$udp_port"
        return 1
      fi
      if [ "$udp_port" != "$tcp_port" ]; then
        log "NAT-PMP assigned different UDP and TCP ports"
        release_mapping "$udp_port"
        release_mapping "$tcp_port"
        return 1
      fi
      printf '%s\n' "$udp_port"
    }

    renew_mapping() {
      expected_port=$1
      udp_port=$(map_port "$expected_port" udp) || return 1
      tcp_port=$(map_port "$expected_port" tcp) || return 1
      if [ "$udp_port" != "$expected_port" ] || [ "$tcp_port" != "$expected_port" ]; then
        log "NAT-PMP changed the assigned port during renewal"
        if [ "$udp_port" != "$expected_port" ]; then
          release_mapping "$udp_port"
        fi
        if [ "$tcp_port" != "$expected_port" ] && [ "$tcp_port" != "$udp_port" ]; then
          release_mapping "$tcp_port"
        fi
        return 1
      fi
      set_qbittorrent "$expected_port" "$wireguard_interface"
    }

    on_exit() {
      trap - EXIT
      cleanup
    }
    trap 'exit 0' INT TERM
    trap on_exit EXIT

    route=$(ip -o route get "$gateway")
    case " $route " in
      *" dev $wireguard_interface "*) ;;
      *) log "Proton gateway is not routed through $wireguard_interface"; exit 1 ;;
    esac

    cleanup
    while true; do
      if port=$(acquire_mapping); then
        current_port=$port
        if open_firewall "$current_port" && set_qbittorrent "$current_port" "$wireguard_interface"; then
          printf '%s\n' "$current_port" > "$status_file"
          log "forwarded TCP and UDP port $current_port"
          while sleep 45; do
            if renew_mapping "$current_port"; then
              log "renewed TCP and UDP port $current_port"
            else
              break
            fi
          done
        fi
      fi

      cleanup
      log "port forwarding unavailable; retrying in 15 seconds"
      sleep 15
    done
  '';
in
{
  options.modules.services.home-dl.ports.qbittorrent = lib.mkOption {
    type = lib.types.port;
    description = "Host loopback port for the qBittorrent qui frontend";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Native qBittorrent VPN credentials require the 1Password systemd credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers = {
      qbtvpn = {
        privateKey = "op://home-ops-prod/protonvpn-dewey/PrivateKey";
        peerPublicKey = "op://home-ops-prod/protonvpn-dewey/PeerPublicKey";
        peerEndpoint = "op://home-ops-prod/protonvpn-dewey/PeerEndpoint";
      };
      qui = {
        oidcClientSecret = "op://home-ops-prod/qbittorrent/oidc-client-secret";
        sessionSecret = "op://home-ops-prod/qbittorrent/qui-session-secret";
      };
    };

    vpnNamespaces.${namespace} = {
      enable = true;
      inherit wireguardConfigFile;
      inherit namespaceAddress bridgeAddress;
      accessibleFrom = [ bridgeAddress ];
      portMappings = [
        {
          from = qbittorrentApiPort;
          to = qbittorrentApiPort;
          protocol = "tcp";
        }
      ];
    };

    # VPN-Confinement uses the integer sysctl form while Tailscale uses a
    # boolean. Normalize the equivalent values so NixOS can merge them.
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = lib.mkForce 1;

    systemd.services.${namespace} = {
      preStart = ''
        umask 0077
        {
          printf '%s\n' '[Interface]'
          printf 'PrivateKey = %s\n' "$(cat "$CREDENTIALS_DIRECTORY/privateKey")"
          printf '%s\n' 'Address = 10.2.0.2/32'
          printf '%s\n' 'DNS = 10.2.0.1'
          printf '%s\n' '[Peer]'
          printf 'PublicKey = %s\n' "$(cat "$CREDENTIALS_DIRECTORY/peerPublicKey")"
          printf '%s\n' 'AllowedIPs = 0.0.0.0/0'
          printf 'Endpoint = %s\n' "$(cat "$CREDENTIALS_DIRECTORY/peerEndpoint")"
          printf '%s\n' 'PersistentKeepalive = 25'
        } > ${wireguardConfigFile}
      '';
      serviceConfig.RuntimeDirectory = namespace;
    };

    services.qbittorrent = {
      enable = true;
      package = pkgs.qbittorrent-nox;
      user = mediaUser;
      group = mediaGroup;
      profileDir = qbittorrentProfileDir;
      webuiPort = qbittorrentApiPort;
      torrentingPort = null;
      openFirewall = false;
      serverConfig = {
        LegalNotice.Accepted = true;
        BitTorrent.Session = {
          DHTEnabled = false;
          FinishedTorrentExportDirectory = qbittorrentFinishedTorrentFilesDir;
          LSDEnabled = false;
          PeXEnabled = false;
          TorrentExportDirectory = qbittorrentTorrentFilesDir;
          TorrentContentLayout = "Subfolder";
        };
        Preferences = {
          Connection = {
            Interface = "lo";
            PortRangeMin = 0;
            RandomPort = false;
            UPnP = false;
          };
          Downloads = {
            SavePath = "${qbittorrentDownloadsDir}/complete/";
            TempPath = "${qbittorrentDownloadsDir}/incomplete/";
            TempPathEnabled = true;
          };
          Queueing = {
            MaxActiveDownloads = 10;
            MaxActiveTorrents = 100;
            MaxActiveUploads = 90;
            QueueingEnabled = true;
          };
          WebUI = {
            Address = "*";
            AuthSubnetWhitelist = "${bridgeAddress}/32";
            AuthSubnetWhitelistEnabled = true;
            HostHeaderValidation = false;
            LocalHostAuth = false;
            Port = qbittorrentApiPort;
            ReverseProxySupportEnabled = true;
            TrustedReverseProxiesList = bridgeAddress;
          };
        };
      };
    };

    systemd.services.qbittorrent = {
      vpnConfinement = {
        enable = true;
        vpnNamespace = namespace;
      };
      serviceConfig = {
        BindPaths = [ "${qbittorrentStateDir}:${qbittorrentProfileDir}" ];
        BindReadOnlyPaths = [
          "${qbittorrentWatchedFolders}:${qbittorrentProfileDir}/qBittorrent/config/watched_folders.json"
        ];
        ReadWritePaths = [
          qbittorrentProfileDir
          downloadsDir
        ];
        UMask = "0007";
      };
    };

    users.users.${quiUser.name} = {
      inherit (quiUser) name uid isSystemUser;
      group = quiGroup.name;
    };
    users.groups.${quiGroup.name} = {
      inherit (quiGroup) gid;
    };

    services.qui = {
      enable = true;
      user = quiUser.name;
      group = quiGroup.name;
      package = pkgs.qui;
      secretFile = onepassword.creds.qui.sessionSecret;
      openFirewall = false;
      settings = {
        host = "127.0.0.1";
        oidcClientId = "eaff8d42-7b05-48b3-9471-df0b2a0165ab";
        oidcDisableBuiltInLogin = false;
        oidcEnabled = true;
        oidcIssuer = "https://id.${cfg.baseDomain}";
        oidcRedirectUrl = "https://${qbittorrentDomain}/api/auth/oidc/callback";
        port = cfg.ports.qbittorrent;
      };
    };

    systemd.services.qui = {
      environment.QUI__OIDC_CLIENT_SECRET_FILE = onepassword.creds.qui.oidcClientSecret;
      serviceConfig = {
        BindPaths = [ "${quiStateDir}:/var/lib/qui" ];
        LoadCredential = lib.mkForce [
          "oidcClientSecret:${onepassword.socketPath}"
          "sessionSecret:${onepassword.socketPath}"
        ];
      };
    };

    systemd.tmpfiles.rules = [
      "d ${qbittorrentStateDir} 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentBlackholeDir} 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentDownloadsDir}/complete 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentDownloadsDir}/incomplete 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentTorrentFilesDir} 0770 ${mediaUser} ${mediaGroup}"
      "d ${qbittorrentFinishedTorrentFilesDir} 0770 ${mediaUser} ${mediaGroup}"
      "d ${quiStateDir} 0750 ${quiUser.name} ${quiGroup.name}"
    ];

    systemd.services.proton-qbittorrent-port-forward = {
      description = "Maintain Proton NAT-PMP forwarding for qBittorrent";
      wantedBy = [ "multi-user.target" ];
      requires = [ "qbittorrent.service" ];
      after = [ "qbittorrent.service" ];
      vpnConfinement = {
        enable = true;
        vpnNamespace = namespace;
      };
      path = [
        pkgs.coreutils
        pkgs.curl
        pkgs.gnugrep
        pkgs.iptables
        pkgs.iproute2
        pkgs.libnatpmp
      ];
      serviceConfig = {
        Type = "simple";
        ExecStart = portForwardScript;
        Restart = "always";
        RestartSec = "15s";
        RuntimeDirectory = "proton-qbittorrent-port-forward";
        UMask = "0077";
        CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
        AmbientCapabilities = [ "CAP_NET_ADMIN" ];
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
          "AF_UNIX"
        ];
      };
    };

    site.gatus.endpoints = [
      {
        name = "qBittorrent";
        group = config.site.gatus.groups.media;
        url = "https://${qbittorrentDomain}/";
      }
    ];

    modules.services.caddy.routes.qbittorrent = {
      publicHost = qbittorrentDomain;
      upstream = "http://127.0.0.1:${toString cfg.ports.qbittorrent}";
      requestBodyMaxSize = "10MB";
    };
  };
}
