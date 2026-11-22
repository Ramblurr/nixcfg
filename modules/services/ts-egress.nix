{
  config,
  lib,
  pkgs,
  ...
}:

# A Tailscale exit node whose every forwarded TCP and UDP flow, DNS included,
# is TPROXY'd into ts-egress (pkgs/ts-egress), which decides it from the
# caller's grant capabilities. Nothing is routed: IP forwarding stays off and
# forwarded packets on the Tailscale interface are dropped, so a stopped
# ts-egress or a missing ruleset fails closed.
let
  cfg = config.modules.services.ts-egress;
  tsIface = config.services.tailscale.interfaceName;
  tproxyMark = 1;
  tproxyTable = 100;
  helperEnabled = cfg.helper.address != null;
  onepasswordEnabled = cfg.onepassword.url != null;
  brokerEnabled = cfg.broker.socket != null;
  # systemd credential IDs; ts-egress reads them from $CREDENTIALS_DIRECTORY.
  credential = {
    caCert = "ca-cert";
    caKey = "ca-key";
    connectToken = "op-connect-token";
    authKey = "ts-authkey";
  };
  configFile = (pkgs.formats.json { }).generate "ts-egress.json" (
    {
      tcpListen = [
        "0.0.0.0:${toString cfg.port}"
        "[::]:${toString cfg.port}"
      ];
      udpListen = [
        "0.0.0.0:${toString cfg.port}"
        "[::]:${toString cfg.port}"
      ];
      tailscaledSocket = "/run/tailscale/tailscaled.sock";
      inherit (cfg) dnsUpstream;
      self = cfg.selfAddresses;
    }
    // lib.optionalAttrs helperEnabled {
      helper.listen = "${cfg.helper.address}:${toString cfg.helper.port}";
    }
    // lib.optionalAttrs cfg.ca.enable {
      ca = {
        certCredential = credential.caCert;
        keyCredential = credential.caKey;
      };
    }
    // lib.optionalAttrs onepasswordEnabled {
      onepassword = {
        inherit (cfg.onepassword) url;
        tokenCredential = credential.connectToken;
      };
    }
    // lib.optionalAttrs brokerEnabled {
      brokerSocket = cfg.broker.socket;
    }
  );
  # The source file of a credential, by default on the MicroVM host-secrets
  # mount. Never the Nix store, which is world-readable.
  secretPath =
    name: description:
    lib.mkOption {
      type = lib.types.addCheck lib.types.str (
        p: lib.hasPrefix "/" p && !lib.hasPrefix builtins.storeDir p
      );
      default = "/run/host-secrets/${name}";
      description = "${description} Loaded as the systemd credential ${name}.";
    };
in
{
  options.modules.services.ts-egress = {
    enable = lib.mkEnableOption "a Tailscale exit node that sends every flow through ts-egress";

    package = lib.mkPackageOption pkgs "ts-egress" { };

    authKeyFile = secretPath credential.authKey "File holding the Tailscale auth key for tailscaled-autoconnect.";

    tag = lib.mkOption {
      type = lib.types.str;
      default = "tag:egress";
      description = "Tag the exit node advertises. Grants route tenants through it with `via`.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 15001;
      description = "Port of the transparent TCP and UDP listeners that TPROXY delivers to.";
    };

    dnsUpstream = lib.mkOption {
      type = lib.types.str;
      example = "192.0.2.1:53";
      description = "Resolver, as host:port, that ts-egress forwards permitted DNS questions to.";
    };

    selfAddresses = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Addresses of this host that ts-egress must deny besides those on its
        interfaces at startup.
      '';
    };

    helper = {
      address = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          IPv4 address for the allow-only CONNECT proxy that tenants' own
          Tailscale nodes use to reach control and DERP. Null disables it.
        '';
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 3128;
        description = "Port of the CONNECT helper.";
      };
      interface = lib.mkOption {
        type = lib.types.str;
        description = "Interface the firewall opens the helper on.";
      };
      allowFrom = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "IPv4 source CIDRs the firewall lets reach the helper.";
      };
    };

    ca = {
      enable = lib.mkEnableOption ''
        decryption of flows that match an http rule, with leaves signed by a
        CA that tenants must trust. Without it those flows are denied
      '';
      certFile = secretPath credential.caCert "File holding the PEM CA certificate.";
      keyFile = secretPath credential.caKey "File holding the PEM private key of the CA.";
    };

    onepassword = {
      url = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          1Password Connect URL that http rules' op:// secrets are fetched
          from. Null leaves only literal secrets. Needs ca.enable.
        '';
      };
      tokenFile = secretPath credential.connectToken "File holding the Connect token.";
    };

    broker = {
      socket = lib.mkOption {
        type = lib.types.nullOr (lib.types.addCheck lib.types.str (lib.hasPrefix "/"));
        default = null;
        example = "/run/ts-broker/decide.sock";
        description = ''
          Unix socket of the credential broker, asked about placeholders no
          http rule lists for callers whose caps carry a broker ceiling.
          Null disables it. Needs onepassword.url.
        '';
      };
      group = lib.mkOption {
        type = lib.types.str;
        default = "ts-broker-clients";
        description = "Group that may write to the broker socket. ts-egress runs with it.";
      };
    };

    auditOutput = lib.mkOption {
      type = lib.types.str;
      default = "journal";
      example = "append:/var/log/ts-egress/audit.jsonl";
      description = "systemd StandardOutput= for the JSON audit log. Diagnostics go to stderr.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.systemd.network.enable;
        message = "modules.services.ts-egress declares its TPROXY route and rule with systemd-networkd.";
      }
      {
        assertion = config.services.resolved.enable;
        message = "modules.services.ts-egress hides resolved's resolv.conf from tailscaled and needs services.resolved.";
      }
      {
        assertion = !(builtins.elem tsIface config.networking.firewall.trustedInterfaces);
        message = "modules.services.ts-egress: ${tsIface} must not be a trusted firewall interface.";
      }
      {
        assertion = onepasswordEnabled -> cfg.ca.enable;
        message = "modules.services.ts-egress.onepassword injects secrets only into decrypted flows and needs ca.enable.";
      }
      {
        assertion = brokerEnabled -> onepasswordEnabled;
        message = "modules.services.ts-egress.broker names op:// secrets and needs onepassword.url.";
      }
    ];

    users.groups = lib.mkIf brokerEnabled { ${cfg.broker.group} = { }; };

    services.tailscale = {
      enable = true;
      authKeyFile = "/run/credentials/tailscaled-autoconnect.service/${credential.authKey}";
      openFirewall = true;
      extraUpFlags = [
        "--advertise-exit-node"
        "--advertise-tags=${cfg.tag}"
        "--accept-dns=false"
      ];
      # up only runs at the first login; set converges the rest on every boot.
      extraSetFlags = [
        "--advertise-exit-node"
        "--accept-dns=false"
      ];
    };

    # TPROXY delivers locally, so the exit node routes nothing. tailscaled
    # reports a forwarding health warning; that is expected.
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = false;
      "net.ipv6.conf.all.forwarding" = false;
    };

    # tailscaled answers exit-node DNS over peerapi DoH inside its netstack,
    # past nftables, using the first nameserver of its /etc/resolv.conf. With
    # resolved's directory hidden that file is missing, so those queries fail
    # and tenants' DNS only resolves through ts-egress. tailscaled's own
    # lookups then fall back to 127.0.0.1:53, which resolved serves.
    systemd.services.tailscaled-autoconnect.serviceConfig.LoadCredential = [
      "${credential.authKey}:${cfg.authKeyFile}"
    ];

    systemd.services.tailscaled.serviceConfig.TemporaryFileSystem = [ "/run/systemd/resolve:ro" ];
    # tailscaled turns on src_valid_mark, so the kernel validates a
    # TPROXY-marked tenant packet's source through table 100, finds a local
    # route and drops it as a martian. accept_local lets it through; the
    # interface only exists once tailscaled runs, so set it from there.
    systemd.services.tailscaled.serviceConfig.ExecStartPost = [
      "${pkgs.procps}/bin/sysctl -w net.ipv4.conf.${tsIface}.accept_local=1"
    ];
    services.resolved.settings.Resolve.DNSStubListenerExtra = [ "127.0.0.1" ];

    systemd.network.networks."40-ts-egress-tproxy" = {
      matchConfig.Name = "lo";
      linkConfig.RequiredForOnline = "no";
      routingPolicyRules = [
        {
          FirewallMark = tproxyMark;
          Table = tproxyTable;
          Priority = 100;
          Family = "both";
        }
      ];
      routes = [
        {
          Type = "local";
          Destination = "0.0.0.0/0";
          Table = tproxyTable;
        }
        {
          Type = "local";
          Destination = "::/0";
          Table = tproxyTable;
        }
      ];
    };

    networking.nftables.enable = true;
    networking.nftables.tables.ts-egress = {
      family = "inet";
      content = ''
        chain prerouting {
          type filter hook prerouting priority mangle; policy accept;
          iifname "${tsIface}" meta l4proto { tcp, udp } tproxy to :${toString cfg.port} meta mark set ${toString tproxyMark} accept
        }
        # Without a ts-egress socket TPROXY leaves a flow unmarked; it must
        # not reach anything else listening on this host.
        chain input {
          type filter hook input priority filter - 10; policy accept;
          iifname "${tsIface}" meta mark != ${toString tproxyMark} ct state != { established, related } counter drop
        }
        chain forward {
          type filter hook forward priority filter; policy accept;
          iifname "${tsIface}" counter drop
          oifname "${tsIface}" counter drop
        }
      '';
    };

    # The TPROXY mark sends tailnet sources to the local table, which strict
    # reverse-path filtering rejects.
    networking.firewall.checkReversePath = "loose";
    # The reverse-path lookup for a TPROXY-marked packet hits table 100's
    # local route and fails, so NixOS's rpfilter drops every tenant flow.
    networking.firewall.extraReversePathFilterRules = ''
      iifname "${tsIface}" meta mark ${toString tproxyMark} accept
    '';
    networking.firewall.extraInputRules = lib.concatStringsSep "\n" (
      [ ''iifname "${tsIface}" meta mark ${toString tproxyMark} accept'' ]
      ++ lib.optional helperEnabled ''iifname "${cfg.helper.interface}" ip saddr { ${lib.concatStringsSep ", " cfg.helper.allowFrom} } tcp dport ${toString cfg.helper.port} accept''
    );

    systemd.services.ts-egress = {
      description = "Tailscale exit-node egress";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      requires = [ "tailscaled.service" ];
      after = [
        "network-online.target"
        "tailscaled.service"
        "tailscaled-autoconnect.service"
        "nftables.service"
      ];
      startLimitIntervalSec = 0;
      serviceConfig = {
        ExecStart = "${lib.getExe cfg.package} -config ${configFile}";
        # A missing source file keeps the unit from starting.
        LoadCredential =
          lib.optionals cfg.ca.enable [
            "${credential.caCert}:${cfg.ca.certFile}"
            "${credential.caKey}:${cfg.ca.keyFile}"
          ]
          ++ lib.optional onepasswordEnabled "${credential.connectToken}:${cfg.onepassword.tokenFile}";
        SupplementaryGroups = lib.optional brokerEnabled cfg.broker.group;
        StandardOutput = cfg.auditOutput;
        Restart = "always";
        RestartSec = 2;

        DynamicUser = true;
        # IP_TRANSPARENT needs CAP_NET_RAW or CAP_NET_ADMIN; NET_RAW cannot
        # change routes or the ruleset. UDP replies are sent from the
        # original destination, so ports such as 53 and 123 need
        # CAP_NET_BIND_SERVICE.
        AmbientCapabilities = [
          "CAP_NET_RAW"
          "CAP_NET_BIND_SERVICE"
        ];
        CapabilityBoundingSet = [
          "CAP_NET_RAW"
          "CAP_NET_BIND_SERVICE"
        ];
        NoNewPrivileges = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          "AF_NETLINK"
        ];
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
        ];
        SystemCallErrorNumber = "EPERM";
        UMask = "0077";
      };
    };
  };
}
