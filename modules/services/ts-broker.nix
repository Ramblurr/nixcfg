{
  config,
  lib,
  pkgs,
  ...
}:

# ts-plug puts the broker on the tailnet as its own node, named by hostname. It
# starts the broker as its child, so both run as the ts-broker user, and
# answers HTTPS on the tailnet by proxying to the UI socket with the
# caller's identity headers. The UI socket and ts-plug's HTTP proxy for
# Connect sit in uiDir, mode 0700, which no other user can traverse: no
# other local process can forge the identity headers or dial the tailnet as
# ts-broker. The broker opens no TCP port and needs no firewall rule.
let
  cfg = config.modules.services.ts-broker;
  connectTokenCredential = "op-connect-token";
  authKeyCredential = "ts-authkey";
  connectEnabled = cfg.onepassword.url != null;
  uiDir = "/run/ts-broker/ui";
  uiSocket = "${uiDir}/ui.sock";
  proxySocket = "${uiDir}/proxy.sock";
  broker = [
    (lib.getExe cfg.package)
    "serve"
    "--db"
    "/var/lib/ts-broker/broker.db"
    "--socket"
    "/run/ts-broker/decide.sock"
    "--ui-socket"
    uiSocket
  ]
  ++ lib.optionals connectEnabled [
    "--connect-url"
    cfg.onepassword.url
    "--connect-proxy"
    proxySocket
  ];
  plug = [
    (lib.getExe cfg.plugPackage)
    "-hostname"
    cfg.hostname
    "-dir"
    "/var/lib/ts-broker-tailscale"
    "-https-port"
    "443:unix:${uiSocket}"
    "-inherit-output"
    "-response-header-timeout"
    "15s"
  ]
  ++ lib.optionals connectEnabled [
    "-http-proxy"
    "unix:${proxySocket}"
  ];
  # tsnet reads the auth key from TS_AUTHKEY for its first login.
  start = pkgs.writeShellScript "ts-broker-start" ''
    TS_AUTHKEY=$(< "$CREDENTIALS_DIRECTORY/${authKeyCredential}")
    export TS_AUTHKEY
    exec ${lib.escapeShellArgs (plug ++ [ "--" ] ++ broker)}
  '';
in
{
  options.modules.services.ts-broker = {
    enable = lib.mkEnableOption "the ts-egress credential approval broker";

    package = lib.mkPackageOption pkgs "ts-broker" { };

    plugPackage = lib.mkPackageOption pkgs "ts-plug" { };

    hostname = lib.mkOption {
      type = lib.types.str;
      default = "ts-broker";
      description = "Tailscale hostname of the broker's node, which serves the approval UI.";
    };

    authKeyFile = lib.mkOption {
      type = lib.types.addCheck lib.types.str (
        p: lib.hasPrefix "/" p && !lib.hasPrefix builtins.storeDir p
      );
      default = "/run/host-secrets/ts-broker-authkey";
      description = ''
        File holding the Tailscale auth key, tagged tag:ts-broker, that the
        ts-broker node logs in with once. Loaded as the systemd credential
        ${authKeyCredential}. Never the Nix store, which is world-readable.
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
    modules.motd.units = [ config.systemd.services.ts-broker.name ];
    users.users.ts-broker = {
      isSystemUser = true;
      group = "ts-broker";
    };
    users.groups.ts-broker = { };
    users.groups.${cfg.socketGroup} = { };

    systemd.services.ts-broker = {
      description = "Credential approval broker for ts-egress";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      startLimitIntervalSec = 0;
      serviceConfig = {
        ExecStartPre = "${pkgs.coreutils}/bin/install -d -m 0700 ${uiDir}";
        ExecStart = start;
        LoadCredential = [
          "${authKeyCredential}:${cfg.authKeyFile}"
        ]
        ++ lib.optional connectEnabled "${connectTokenCredential}:${cfg.onepassword.tokenFile}";
        StandardOutput = cfg.auditOutput;
        Restart = "always";
        RestartSec = 2;

        User = "ts-broker";
        # New files, the socket among them, take this group.
        Group = cfg.socketGroup;
        StateDirectory = [
          "ts-broker"
          "ts-broker-tailscale"
        ];
        StateDirectoryMode = "0700";
        RuntimeDirectory = "ts-broker";
        RuntimeDirectoryMode = "0750";
        UMask = "0077";

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
      };
    };
  };
}
