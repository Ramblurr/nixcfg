{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.immich-ml-proxy;
  serviceName = "immich-ml-${cfg.role}-proxy";
  credentialDirectory = "/run/credentials/${serviceName}.service";
  credentialPath = name: "${credentialDirectory}/${name}";
  expiryServiceName = "${serviceName}-certificate-expiry";
  expiryCredentialDirectory = "/run/credentials/${expiryServiceName}.service";
  expiryCredentialPath = name: "${expiryCredentialDirectory}/${name}";
  monitoredCertificateNames = builtins.attrNames cfg.monitoredCertificates;
  expiryCredentialLoads = lib.mapAttrsToList (
    name: source: "${name}:${source}"
  ) cfg.monitoredCertificates;
  expiryCredentialSourcesAreValid = lib.all (
    source: lib.hasPrefix "/" source && !lib.hasPrefix builtins.storeDir source
  ) (builtins.attrValues cfg.monitoredCertificates);
  expiryCheck = pkgs.writeShellScript "${expiryServiceName}" ''
    set -eu
    failed=0
    for certificate_name in ${
      lib.concatMapStringsSep " " lib.escapeShellArg monitoredCertificateNames
    }; do
      certificate="$CREDENTIALS_DIRECTORY/$certificate_name"
      if ! ${lib.getExe' pkgs.openssl "openssl"} x509 -checkend ${
        toString (cfg.certificateExpiryWarningDays * 24 * 60 * 60)
      } -noout -in "$certificate"; then
        echo "Certificate $(basename "$certificate") expires in less than ${toString cfg.certificateExpiryWarningDays} days" >&2
        failed=1
      fi
    done
    exit "$failed"
  '';
  authorizedCertificateNames = builtins.attrNames cfg.authorizedClientCertificates;
  credentialSources = {
    "ca.pem" = cfg.credentials.ca;
    "certificate.pem" = cfg.credentials.certificate;
    "private-key.pem" = cfg.credentials.privateKey;
  }
  // cfg.authorizedClientCertificates;
  credentialLoads = lib.mapAttrsToList (name: source: "${name}:${source}") (
    lib.filterAttrs (_: source: source != null) credentialSources
  );
  credentialSourcesAreValid = lib.all (
    source: source == null || (lib.hasPrefix "/" source && !lib.hasPrefix builtins.storeDir source)
  ) (builtins.attrValues credentialSources);
  ipv4Sources = lib.filter (source: !(lib.hasInfix ":" source)) cfg.allowedSourceAddresses;
  ipv6Sources = lib.filter (source: lib.hasInfix ":" source) cfg.allowedSourceAddresses;
  addressSet = addresses: "{ ${lib.concatStringsSep ", " addresses} }";
  serverFirewallRules = lib.concatStringsSep "\n" (
    lib.optionals (ipv4Sources != [ ]) [
      (
        if cfg.listenAddress == "0.0.0.0" then
          "ip saddr ${addressSet ipv4Sources} tcp dport ${toString cfg.port} accept"
        else
          "ip saddr ${addressSet ipv4Sources} ip daddr ${cfg.listenAddress} tcp dport ${toString cfg.port} accept"
      )
    ]
    ++ lib.optionals (ipv6Sources != [ ]) [
      "ip6 saddr ${addressSet ipv6Sources} tcp dport ${toString cfg.port} accept"
    ]
  );
  clientCaddyfile = ''
    {
      admin off
      auto_https off
    }

    http://127.0.0.1:${toString cfg.port}, http://[::1]:${toString cfg.port} {
      reverse_proxy https://${cfg.upstreamAddress}:${toString cfg.upstreamPort} {
        header_up Host ${cfg.serverName}
        transport http {
          tls_server_name ${cfg.serverName}
          tls_trust_pool file {
            pem_file ${credentialPath "ca.pem"}
          }
          tls_client_auth ${credentialPath "certificate.pem"} ${credentialPath "private-key.pem"}
        }
      }
    }
  '';
  serverCaddyfile = ''
    {
      admin off
      auto_https off
    }

    https://${cfg.serverName}:${toString cfg.port} {
      bind ${cfg.listenAddress}
      tls ${credentialPath "certificate.pem"} ${credentialPath "private-key.pem"} {
        client_auth {
          mode require_and_verify
          trust_pool file {
            pem_file ${credentialPath "ca.pem"}
          }
          verifier leaf {
            file ${lib.concatMapStringsSep " " credentialPath authorizedCertificateNames}
          }
        }
      }
      reverse_proxy http://${cfg.upstreamAddress}:${toString cfg.upstreamPort}
    }
  '';
  caddyfile = pkgs.writeText "${serviceName}.Caddyfile" (
    if cfg.role == "client" then clientCaddyfile else serverCaddyfile
  );
in
{
  options.modules.services.immich-ml-proxy = {
    enable = lib.mkEnableOption "an authenticated Immich machine-learning proxy";

    role = lib.mkOption {
      type = lib.types.enum [
        "client"
        "server"
      ];
      default = "client";
      description = "Whether this host accepts local Immich calls or protects the remote ML service.";
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = if cfg.role == "client" then "127.0.0.1" else "127.0.0.1";
      description = "Server listener address. Client listeners are fixed to IPv4 and IPv6 loopback.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = if cfg.role == "client" then 3004 else 3443;
      description = "Local client-proxy or protected server-proxy port.";
    };

    upstreamAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Remote protected endpoint for a client, or loopback ML endpoint for a server.";
    };

    upstreamPort = lib.mkOption {
      type = lib.types.port;
      default = 3003;
      description = "Remote protected port for a client, or loopback ML port for a server.";
    };

    serverName = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "TLS server identity verified by a client proxy.";
    };

    allowedUser = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Only this local user may connect to a client proxy.";
    };

    allowedSourceAddresses = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Source addresses allowed through the host firewall to a server proxy.";
    };

    credentials = {
      ca = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Runtime path to the private CA certificate.";
      };
      certificate = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Runtime path to this proxy's certificate.";
      };
      privateKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Runtime path to this proxy's private key.";
      };
    };
    loadCredentials = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Load credentials from the configured paths. Disable this when another
        module supplies the same credential IDs directly to the proxy service.
      '';
    };

    monitoredCertificates = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Runtime certificate paths checked daily for approaching expiry on a server proxy.";
    };

    loadMonitoringCredentials = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Load monitored certificates from their configured paths.";
    };

    certificateExpiryWarningDays = lib.mkOption {
      type = lib.types.ints.positive;
      default = 30;
      description = "Days before expiry when the certificate monitoring unit fails.";
    };

    authorizedClientCertificates = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Runtime client certificate paths accepted by a server proxy, keyed by credential ID.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = credentialSourcesAreValid;
        message = "Immich ML proxy credentials must be absolute runtime paths outside the Nix store.";
      }
      {
        assertion =
          cfg.loadCredentials
          || lib.all (name: credentialSources.${name} == credentialPath name) (
            builtins.attrNames credentialSources
          );
        message = "Externally supplied Immich ML credentials must use the proxy service credential directory.";
      }
      {
        assertion =
          cfg.credentials.ca != null
          && cfg.credentials.certificate != null
          && cfg.credentials.privateKey != null;
        message = "The Immich ML proxy requires CA, certificate, and private-key credentials.";
      }
      {
        assertion = cfg.serverName != null;
        message = "An Immich ML proxy requires a TLS server name.";
      }
      {
        assertion =
          cfg.role != "client"
          || (
            cfg.allowedUser != null
            && builtins.hasAttr cfg.allowedUser config.users.users
            && config.users.users.${cfg.allowedUser}.uid != null
          );
        message = "An Immich ML client proxy requires an existing allowed user with a fixed UID.";
      }
      {
        assertion = cfg.role != "server" || cfg.authorizedClientCertificates != { };
        message = "An Immich ML server proxy requires at least one authorized client certificate.";
      }
      {
        assertion = lib.all (
          name: builtins.match "[A-Za-z0-9_.-]+" name != null
        ) authorizedCertificateNames;
        message = "Immich ML authorized client credential IDs contain invalid characters.";
      }
      {
        assertion = cfg.role != "server" || cfg.monitoredCertificates != { };
        message = "An Immich ML server proxy requires certificate expiry monitoring.";
      }
      {
        assertion = expiryCredentialSourcesAreValid;
        message = "Monitored Immich ML certificates must be absolute runtime paths outside the Nix store.";
      }
      {
        assertion =
          cfg.loadMonitoringCredentials
          || lib.all (name: cfg.monitoredCertificates.${name} == expiryCredentialPath name) (
            builtins.attrNames cfg.monitoredCertificates
          );
        message = "Externally supplied monitored certificates must use the expiry service credential directory.";
      }
      {
        assertion = lib.all (name: builtins.match "[A-Za-z0-9_.-]+" name != null) (
          builtins.attrNames cfg.monitoredCertificates
        );
        message = "Immich ML monitored certificate credential IDs contain invalid characters.";
      }
      {
        assertion = cfg.role != "server" || cfg.allowedSourceAddresses != [ ];
        message = "An Immich ML server proxy requires at least one firewall source address.";
      }
    ];

    users.users.${serviceName} = {
      isSystemUser = true;
      group = serviceName;
    };
    users.groups.${serviceName} = { };

    systemd.services.${serviceName} = {
      description = "Authenticated Immich machine-learning ${cfg.role} proxy";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      environment = {
        HOME = "/run/${serviceName}";
        XDG_CONFIG_HOME = "/run/${serviceName}/config";
        XDG_DATA_HOME = "/run/${serviceName}/data";
      };
      serviceConfig = {
        ExecStartPre = "${lib.getExe pkgs.caddy} validate --config ${caddyfile} --adapter caddyfile";
        ExecStart = "${lib.getExe pkgs.caddy} run --config ${caddyfile} --adapter caddyfile";
        LoadCredential = lib.optionals cfg.loadCredentials credentialLoads;
        User = serviceName;
        Group = serviceName;
        RuntimeDirectory = serviceName;
        RuntimeDirectoryMode = "0700";
        Restart = "on-failure";
        RestartSec = 3;
        UMask = "0077";
        CapabilityBoundingSet = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProcSubset = "pid";
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "strict";
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
      };
    };
    systemd.services.${expiryServiceName} = lib.mkIf (cfg.role == "server") {
      description = "Check Immich machine-learning certificates for approaching expiry";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = expiryCheck;
        LoadCredential = lib.optionals cfg.loadMonitoringCredentials expiryCredentialLoads;
        User = serviceName;
        Group = serviceName;
        UMask = "0077";
        CapabilityBoundingSet = "";
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
      };
    };

    systemd.timers.${expiryServiceName} = lib.mkIf (cfg.role == "server") {
      description = "Daily Immich machine-learning certificate expiry check";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "4h";
      };
    };

    networking.firewall.extraInputRules = lib.mkIf (cfg.role == "server") serverFirewallRules;

    networking.nftables = {
      enable = true;
      tables.immich-ml-client-access = lib.mkIf (cfg.role == "client") {
        family = "inet";
        content = ''
          chain restrict-local-proxy {
            type filter hook output priority -10; policy accept;
            meta skuid ${
              toString config.users.users.${cfg.allowedUser}.uid
            } ip daddr 127.0.0.1 tcp dport ${toString cfg.port} accept
            ip daddr 127.0.0.1 tcp dport ${toString cfg.port} reject with tcp reset
            meta skuid ${
              toString config.users.users.${cfg.allowedUser}.uid
            } ip6 daddr ::1 tcp dport ${toString cfg.port} accept
            ip6 daddr ::1 tcp dport ${toString cfg.port} reject with tcp reset
          }
        '';
      };
    };
  };
}
