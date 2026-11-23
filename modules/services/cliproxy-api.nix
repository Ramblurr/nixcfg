{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.services.cliproxy-api;
  authKeyCredential = "ts-authkey";
  stateDir = "/var/lib/cliproxyapi";
  configFile = "/run/cliproxyapi/config.yaml";
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
  # op:// references hold characters a systemd credential ID cannot.
  credentialIds = lib.listToAttrs (
    lib.imap0 (i: reference: lib.nameValuePair reference "op-${toString i}") (
      builtins.attrNames cfg.credentials
    )
  );
  reader = pkgs.writeShellScript "cliproxyapi-op-read" ''
    case "$1" in
    ${
      lib.concatStrings (
        lib.mapAttrsToList (reference: id: ''
          ${lib.escapeShellArg reference}) exec ${pkgs.coreutils}/bin/cat "$CREDENTIALS_DIRECTORY/${id}" ;;
        '') credentialIds
      )
    }*) exit 1 ;;
    esac
  '';
  opReferences =
    value:
    if builtins.isAttrs value then
      lib.concatMap opReferences (builtins.attrValues value)
    else if builtins.isList value then
      lib.concatMap opReferences value
    else
      lib.optional (builtins.isString value && lib.hasPrefix "op://" value) value;
  unmapped = lib.subtractLists (builtins.attrNames cfg.credentials) (opReferences cfg.settings);
  render = pkgs.writeShellScript "cliproxyapi-render" ''
    set -eu
    exec ${bb} ${../../scripts/render-op-config.clj} ${template} ${reader} ${configFile}
  '';
  plug = [
    (lib.getExe cfg.plugPackage)
    "-hostname"
    cfg.hostname
    "-dir"
    "/var/lib/cliproxyapi-tailscale"
    "-https-port"
    "443:${toString cfg.port}"
    "-inherit-output"
    # Non-streaming completions send no headers until the model finishes.
    "-response-header-timeout"
    "0"
  ];
  start = pkgs.writeShellScript "cliproxyapi-start" ''
    set -eu
    TS_AUTHKEY=$(< "$CREDENTIALS_DIRECTORY/${authKeyCredential}")
    export TS_AUTHKEY
    exec ${
      lib.escapeShellArgs (
        plug
        ++ [
          "--"
          (lib.getExe cfg.package)
          "--config"
          configFile
        ]
      )
    }
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

    credentials = lib.mkOption {
      type = lib.types.attrsOf secretPath;
      default = { };
      example = {
        "op://vault/OpenRouter/credential" = "/run/host-secrets/cliproxy-openrouter-key";
      };
      description = ''
        File holding the value of each op:// reference in settings, loaded
        as a systemd credential. Never the Nix store, which is world-readable.
      '';
    };

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = ''
        CLIProxyAPI settings. At each start, exact op:// strings are replaced
        with the content of their credentials file and written to
        ${configFile}, mode 0600; never put secret values here. A blank value
        is dropped, and the service fails when no access.api-keys entry has one.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.any (k: builtins.isString k && builtins.match ".*[!-~].*" k != null) (
          cfg.settings.access.api-keys or [ ]
        );
        message = "modules.services.cliproxy-api.settings.access.api-keys must configure at least one non-blank client API key.";
      }
      {
        assertion = unmapped == [ ];
        message = "modules.services.cliproxy-api.credentials has no file for: ${lib.concatStringsSep ", " unmapped}";
      }
    ];

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
        ExecStartPre = render;
        ExecStart = start;
        LoadCredential = [
          "${authKeyCredential}:${cfg.authKeyFile}"
        ]
        ++ lib.mapAttrsToList (reference: file: "${credentialIds.${reference}}:${file}") cfg.credentials;
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
