{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.ts-broker;
  connectTokenCredential = "op-connect-token";
  connectEnabled = cfg.onepassword.url != null;
in
{
  options.modules.services.ts-broker = {
    enable = lib.mkEnableOption "the ts-egress credential approval broker";

    package = lib.mkPackageOption pkgs "ts-broker" { };

    port = lib.mkOption {
      type = lib.types.ints.between 1 1023;
      default = 795;
      description = ''
        Port of the UI on 127.0.0.1. Every request needs the
        Tailscale-User-Login header. The port is privileged, so no other
        user can listen on it while the broker is down.
      '';
    };

    socketGroup = lib.mkOption {
      type = lib.types.str;
      default = "ts-broker-clients";
      description = "Group that owns /run/ts-broker/decide.sock. Its members can ask for decisions.";
    };

    onepassword = {
      url = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "1Password Connect URL the catalog reads vaults and items from. Null leaves the catalog empty.";
      };
      tokenFile = lib.mkOption {
        type = lib.types.addCheck lib.types.str (
          p: lib.hasPrefix "/" p && !lib.hasPrefix builtins.storeDir p
        );
        default = "/run/host-secrets/ts-broker-op-connect-token";
        description = ''
          File holding the Connect token of the broker, loaded as the systemd
          credential ${connectTokenCredential}. The token decides which items
          the catalog offers. Never the Nix store, which is world-readable.
        '';
      };
    };

    auditOutput = lib.mkOption {
      type = lib.types.str;
      default = "journal";
      example = "append:/var/log/ts-broker/audit.jsonl";
      description = "systemd StandardOutput= for the JSON audit log. Diagnostics go to stderr.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.ts-broker = {
      isSystemUser = true;
      group = "ts-broker";
    };
    users.groups.ts-broker = { };
    users.groups.${cfg.socketGroup} = { };

    # The UI trusts Tailscale-User-Login, so only tailscaled, which runs as
    # root and proxies tailscale serve, may connect to it.
    networking.nftables.enable = true;
    networking.nftables.tables.ts-broker = {
      family = "inet";
      content = ''
        chain output {
          type filter hook output priority filter; policy accept;
          oifname "lo" tcp dport ${toString cfg.port} meta skuid != 0 counter drop
        }
      '';
    };

    systemd.services.ts-broker = {
      description = "Credential approval broker for ts-egress";
      wantedBy = [
        "multi-user.target"
        "nftables.service"
      ];
      wants = [ "network-online.target" ];
      bindsTo = [ "nftables.service" ];
      after = [
        "network-online.target"
        "nftables.service"
      ];
      startLimitIntervalSec = 0;
      serviceConfig = {
        ExecStart = lib.escapeShellArgs (
          [
            (lib.getExe cfg.package)
            "serve"
            "--db"
            "/var/lib/ts-broker/broker.db"
            "--socket"
            "/run/ts-broker/decide.sock"
            "--port"
            (toString cfg.port)
          ]
          ++ lib.optionals connectEnabled [
            "--connect-url"
            cfg.onepassword.url
          ]
        );
        LoadCredential = lib.optional connectEnabled "${connectTokenCredential}:${cfg.onepassword.tokenFile}";
        StandardOutput = cfg.auditOutput;
        Restart = "always";
        RestartSec = 2;

        User = "ts-broker";
        # New files, the socket among them, take this group.
        Group = cfg.socketGroup;
        StateDirectory = "ts-broker";
        StateDirectoryMode = "0700";
        RuntimeDirectory = "ts-broker";
        RuntimeDirectoryMode = "0750";
        UMask = "0077";

        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        NoNewPrivileges = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
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
      };
    };
  };
}
