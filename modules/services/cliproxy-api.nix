{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

# ts-plug puts CLIProxyAPI on the tailnet as its own node and starts it as
# its child, so both run as the cliproxyapi user. The child waits for
# ts-plug's HTTP proxy, which opens once the node is up, resolves the op://
# references in settings through it from 1Password Connect, and then serves
# on loopback. A config without a client key never starts: CLIProxyAPI
# accepts every request when it has none. The proxy socket and the rendered
# config sit in the runtime directory, mode 0700.
let
  cfg = config.modules.services.cliproxy-api;
  authKeyCredential = "ts-authkey";
  connectTokenCredential = "op-connect-token";
  stateDir = "/var/lib/cliproxyapi";
  runDir = "/run/cliproxyapi";
  proxySocket = "${runDir}/proxy.sock";
  configFile = "${runDir}/config.yaml";
  bb = lib.getExe pkgs.babashka;
  template = pkgs.writeText "cliproxyapi-template.json" (
    builtins.toJSON {
      settings = lib.recursiveUpdate {
        config-version = 8;
        server = {
          host = "127.0.0.1";
          inherit (cfg) port;
        };
      } cfg.settings;
    }
  );
  reader = pkgs.writeShellScript "cliproxyapi-op-read" ''
    exec ${bb} ${../../scripts/op-connect-read.clj} ${lib.escapeShellArg cfg.onepassword.url} \
      "$CREDENTIALS_DIRECTORY/${connectTokenCredential}" ${proxySocket} "$1"
  '';
  serve = pkgs.writeShellScript "cliproxyapi-serve" ''
    set -eu
    unset TS_AUTHKEY
    until [ -S ${proxySocket} ]; do sleep 1; done
    ${bb} ${../../scripts/render-op-config.clj} ${template} ${reader} ${configFile}
    exec ${lib.getExe cfg.package} --config ${configFile}
  '';
  plug = [
    (lib.getExe cfg.plugPackage)
    "-hostname"
    cfg.hostname
    "-dir"
    "/var/lib/cliproxyapi-tailscale"
    "-https-port"
    "443:${toString cfg.port}"
    "-http-proxy"
    "unix:${proxySocket}"
    "-inherit-output"
    # Non-streaming completions send no headers until the model finishes.
    "-response-header-timeout"
    "0"
  ];
  # tsnet reads the auth key from TS_AUTHKEY for its first login.
  start = pkgs.writeShellScript "cliproxyapi-start" ''
    TS_AUTHKEY=$(< "$CREDENTIALS_DIRECTORY/${authKeyCredential}")
    export TS_AUTHKEY
    exec ${lib.escapeShellArgs (plug ++ [ "--" ])} ${serve}
  '';
  secretPath = lib.types.addCheck lib.types.str (
    p: lib.hasPrefix "/" p && !lib.hasPrefix builtins.storeDir p
  );
in
{
  options.modules.services.cliproxy-api = {
    enable = lib.mkEnableOption "CLIProxyAPI as a system service on its own tailnet node";

    package = lib.mkOption {
      type = lib.types.package;
      default = import ../../pkgs/cli-proxy-api.nix {
        inherit (inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}) cli-proxy-api;
      };
      defaultText = lib.literalExpression "our CLIProxyAPI fork, pkgs/cli-proxy-api.nix";
      description = "The CLIProxyAPI package.";
    };

    plugPackage = lib.mkPackageOption pkgs "ts-plug" { };

    hostname = lib.mkOption {
      type = lib.types.str;
      default = "moot-llm";
      description = "Tailscale hostname of the node. It serves HTTPS on 443.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8317;
      description = "Loopback port CLIProxyAPI listens on and the node proxies to.";
    };

    authKeyFile = lib.mkOption {
      type = secretPath;
      default = "/run/host-secrets/cliproxy-authkey";
      description = ''
        File holding the Tailscale auth key the node logs in with once.
        Loaded as the systemd credential ${authKeyCredential}. Never the Nix
        store, which is world-readable.
      '';
    };

    onepassword = {
      url = lib.mkOption {
        type = lib.types.str;
        example = "http://op-connect.example.ts.net:8080";
        description = "1Password Connect URL, reached through the node, that op:// references resolve from.";
      };
      tokenFile = lib.mkOption {
        type = secretPath;
        default = "/run/host-secrets/cliproxy-op-connect-token";
        description = ''
          File holding the Connect token, loaded as the systemd credential
          ${connectTokenCredential}. Never the Nix store, which is world-readable.
        '';
      };
    };

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = ''
        CLIProxyAPI settings. Exact op:// strings are resolved at each start
        into ${configFile}, mode 0600; never put secret values here. A
        reference that does not resolve is dropped, and the service fails
        when no access.api-keys entry resolves.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.cliproxyapi = {
      isSystemUser = true;
      group = "cliproxyapi";
    };
    users.groups.cliproxyapi = { };

    systemd.services.cliproxy-api = {
      description = "CLIProxyAPI on its own tailnet node";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      startLimitIntervalSec = 0;
      # CLIProxyAPI keeps OAuth auth files, kv.db and logs under
      # $XDG_DATA_HOME/cliproxyapi and $XDG_STATE_HOME/cliproxyapi.
      environment = {
        HOME = stateDir;
        XDG_DATA_HOME = "/var/lib";
        XDG_STATE_HOME = "/var/lib";
      };
      serviceConfig = {
        ExecStart = start;
        LoadCredential = [
          "${authKeyCredential}:${cfg.authKeyFile}"
          "${connectTokenCredential}:${cfg.onepassword.tokenFile}"
        ];
        Restart = "always";
        RestartSec = 5;

        User = "cliproxyapi";
        Group = "cliproxyapi";
        StateDirectory = [
          "cliproxyapi"
          "cliproxyapi-tailscale"
        ];
        StateDirectoryMode = "0700";
        RuntimeDirectory = "cliproxyapi";
        RuntimeDirectoryMode = "0700";
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
