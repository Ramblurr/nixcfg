{
  config,
  lib,
  pkgs,
  ...
}:

# ts-egress (pkgs/ts-egress) is a Tailscale exit node of its own, built on
# tsnet. Every TCP and UDP flow a tenant sends through it, DNS included,
# arrives in its userspace network stack and is decided from the caller's
# grant capabilities. It needs no tailscaled, no kernel routing and no packet
# redirection: a stopped ts-egress carries nothing.
let
  cfg = config.modules.services.ts-egress;
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
      inherit (cfg) hostname port dnsUpstream;
      stateDir = "/var/lib/ts-egress";
      authKeyCredential = credential.authKey;
      tags = [ cfg.tag ];
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
    enable = lib.mkEnableOption "ts-egress, a Tailscale exit node that decides every flow";

    package = lib.mkPackageOption pkgs "ts-egress" { };

    authKeyFile = secretPath credential.authKey "File holding the Tailscale auth key the node logs in with once.";

    hostname = lib.mkOption {
      type = lib.types.str;
      default = "egress";
      description = "Tailscale hostname of the exit node. Tenants select it with `--exit-node`.";
    };

    tag = lib.mkOption {
      type = lib.types.str;
      default = "tag:egress";
      description = "Tag the exit node advertises. Grants route tenants through it with `via`.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 41641;
      description = "UDP port of the node's WireGuard and peer-to-peer traffic. The firewall opens it.";
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
    modules.motd.units = [ config.systemd.services.ts-egress.name ];
    assertions = [
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

    networking.firewall.allowedUDPPorts = [ cfg.port ];
    networking.nftables.enable = lib.mkIf helperEnabled true;
    networking.firewall.extraInputRules = lib.mkIf helperEnabled ''
      iifname "${cfg.helper.interface}" ip saddr { ${lib.concatStringsSep ", " cfg.helper.allowFrom} } tcp dport ${toString cfg.helper.port} accept
    '';

    systemd.services.ts-egress = {
      description = "Tailscale exit-node egress";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      startLimitIntervalSec = 0;
      serviceConfig = {
        ExecStart = "${lib.getExe cfg.package} -config ${configFile}";
        # A missing source file keeps the unit from starting.
        LoadCredential = [
          "${credential.authKey}:${cfg.authKeyFile}"
        ]
        ++ lib.optionals cfg.ca.enable [
          "${credential.caCert}:${cfg.ca.certFile}"
          "${credential.caKey}:${cfg.ca.keyFile}"
        ]
        ++ lib.optional onepasswordEnabled "${credential.connectToken}:${cfg.onepassword.tokenFile}";
        SupplementaryGroups = lib.optional brokerEnabled cfg.broker.group;
        StandardOutput = cfg.auditOutput;
        Restart = "always";
        RestartSec = 2;

        DynamicUser = true;
        StateDirectory = "ts-egress";
        CapabilityBoundingSet = "";
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
