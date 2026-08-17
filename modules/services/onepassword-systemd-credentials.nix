{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.onepassword-systemd-credentials;
  targetHosts = [
    "debord"
    "dewey"
    "mali"
  ];
  bootstrapServiceNames = [
    "NetworkManager"
    "keepalived"
    "network-addresses"
    "network-setup"
    "networking"
    "onepassword-credential-provider@"
    "op-connect-api"
    "op-connect-sync"
    "sops-install-secrets"
    "systemd-networkd"
    "systemd-networkd-wait-online"
  ];
  tokenSecretName = "onepassword-connect/token";
  tokenFile =
    if cfg.bootstrapTokenFile == null then
      config.sops.secrets.${tokenSecretName}.path
    else
      cfg.bootstrapTokenFile;
  authorizationMap = lib.mapAttrs' (
    service: credentials: lib.nameValuePair "${service}.service" credentials
  ) cfg.consumers;
  authorizationMapFile = pkgs.writeText "onepassword-credential-map.json" (
    builtins.toJSON authorizationMap
  );
  provider = pkgs.writeTextFile {
    name = "onepassword-credential-provider";
    destination = "/bin/onepassword-credential-provider";
    executable = true;
    text = ''
      #!${pkgs.python3}/bin/python3
      import json
      import os
      from pathlib import Path
      import re
      import subprocess
      import sys

      MAX_CREDENTIAL_SIZE = 1024 * 1024


      def fail(message):
          print(f"onepassword-credential-provider: {message}", file=sys.stderr, flush=True)
          raise SystemExit(1)


      remote = os.environ.get("REMOTE_ADDR", "")
      match = re.fullmatch(r"@[^/]+/unit/([^/]+)/([^/]+)", remote)
      if match is None:
          fail("request did not contain a valid systemd unit and credential ID")

      unit_name, credential_id = match.groups()
      credentials_directory = os.environ.get("CREDENTIALS_DIRECTORY")
      if not credentials_directory:
          fail("CREDENTIALS_DIRECTORY is not set")

      credentials = Path(credentials_directory)
      try:
          credential_map = json.loads(
              (credentials / "credential-map").read_text(encoding="utf-8")
          )
          reference = credential_map[unit_name][credential_id]
      except FileNotFoundError:
          fail("provider credentials are unavailable")
      except (json.JSONDecodeError, TypeError):
          fail("credential authorization map is invalid")
      except KeyError:
          fail(f"request for {unit_name}/{credential_id} is not authorized")

      if not isinstance(reference, str) or not reference.startswith("op://"):
          fail(f"authorization for {unit_name}/{credential_id} is invalid")

      try:
          token = (credentials / "connect-token").read_text(encoding="utf-8").rstrip("\r\n")
      except (FileNotFoundError, UnicodeDecodeError):
          fail("Connect bootstrap token is unavailable")

      if not token:
          fail("Connect bootstrap token is empty")

      environment = os.environ.copy()
      environment.update(
          {
              "OP_CACHE": "false",
              "OP_CONNECT_HOST": ${builtins.toJSON cfg.connectHost},
              "OP_CONNECT_TOKEN": token,
          }
      )

      try:
          result = subprocess.run(
              [${builtins.toJSON (lib.getExe cfg.package)}, "read", "--no-newline", reference],
              stdin=subprocess.DEVNULL,
              stdout=subprocess.PIPE,
              stderr=subprocess.DEVNULL,
              env=environment,
              check=False,
              timeout=25,
          )
      except (OSError, subprocess.SubprocessError):
          fail(f"lookup failed for {unit_name}/{credential_id}")

      if result.returncode != 0:
          fail(f"lookup failed for {unit_name}/{credential_id}")
      if not result.stdout:
          fail(f"lookup returned an empty value for {unit_name}/{credential_id}")
      if len(result.stdout) > MAX_CREDENTIAL_SIZE:
          fail(f"lookup exceeded the credential size limit for {unit_name}/{credential_id}")

      sys.stdout.buffer.write(result.stdout)
      sys.stdout.buffer.flush()
    '';
  };
  consumerServices = lib.mapAttrs (_: credentials: {
    requires = [ "onepassword-credential-provider.socket" ];
    after = [ "onepassword-credential-provider.socket" ];
    serviceConfig = {
      LoadCredential = lib.mapAttrsToList (
        credentialId: _: "${credentialId}:${cfg.socketPath}"
      ) credentials;
      ExecStartPre = lib.mapAttrsToList (
        credentialId: _: "${pkgs.coreutils}/bin/test -s %d/${credentialId}"
      ) credentials;
    };
  }) cfg.consumers;
in
{
  options.modules.services.onepassword-systemd-credentials = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = builtins.elem config.networking.hostName targetHosts;
      description = "Whether to provide 1Password-backed credentials to system services.";
    };

    connectHost = lib.mkOption {
      type = lib.types.str;
      default = "http://${builtins.head config.site.net.mgmt.hosts4.onepassword-connect}:8080";
      description = "Connect availability endpoint used for credential lookups.";
    };

    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "/run/onepassword-credential-provider.sock";
      description = "Root-owned UNIX socket used as the systemd credential source.";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs._1password-cli;
      defaultText = lib.literalExpression "pkgs._1password-cli";
      description = "1Password CLI package used to resolve op:// references.";
    };

    bootstrapTokenSopsFile = lib.mkOption {
      type = lib.types.path;
      default = ../../configs/home-ops/shared.sops.yml;
      description = "SOPS document containing onepassword-connect/token.";
    };

    bootstrapTokenFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "/run/credentials/onepassword-bootstrap/token";
      description = ''
        Optional externally managed runtime Connect token. When null, the module
        declares onepassword-connect/token from bootstrapTokenSopsFile.
      '';
    };

    consumers = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf (lib.types.strMatching "^op://.+"));
      default = { };
      example = {
        example.password = "op://home-ops-prod/Example/password";
      };
      description = ''
        Exact system service, credential ID, and op:// reference authorization
        map. Service names omit the .service suffix.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasPrefix "/" cfg.socketPath;
        message = "The 1Password credential provider socket path must be absolute.";
      }
      {
        assertion = cfg.bootstrapTokenFile == null || lib.hasPrefix "/" cfg.bootstrapTokenFile;
        message = "The external 1Password provider bootstrap token path must be absolute.";
      }
      {
        assertion =
          cfg.bootstrapTokenFile == null || !lib.hasPrefix builtins.storeDir cfg.bootstrapTokenFile;
        message = "The 1Password provider bootstrap token must not come from the Nix store.";
      }
      {
        assertion = lib.all (service: !(lib.hasSuffix ".service" service)) (
          builtins.attrNames cfg.consumers
        );
        message = "1Password credential consumer names must omit the .service suffix.";
      }
      {
        assertion = lib.intersectLists bootstrapServiceNames (builtins.attrNames cfg.consumers) == [ ];
        message = "Bootstrap-critical services cannot use the 1Password credential provider.";
      }
      {
        assertion = lib.all (
          credentials:
          lib.all (credentialId: builtins.match "[A-Za-z0-9_.-]+" credentialId != null) (
            builtins.attrNames credentials
          )
        ) (builtins.attrValues cfg.consumers);
        message = "1Password credential IDs may contain only letters, digits, dot, underscore, and hyphen.";
      }
    ];

    sops.secrets.${tokenSecretName} = lib.mkIf (cfg.bootstrapTokenFile == null) {
      sopsFile = cfg.bootstrapTokenSopsFile;
      owner = "root";
      group = "root";
      mode = "0400";
      restartUnits = [ ];
    };

    systemd.sockets.onepassword-credential-provider = {
      description = "1Password-backed systemd credential provider";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = cfg.socketPath;
        Accept = true;
        SocketMode = "0600";
        RemoveOnStop = true;
        MaxConnections = 64;
      };
    };

    systemd.services = lib.mkMerge [
      {
        "onepassword-credential-provider@" = {
          description = "Resolve a systemd credential from 1Password Connect";
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          unitConfig.CollectMode = "inactive-or-failed";
          serviceConfig = {
            Type = "exec";
            ExecStart = lib.getExe provider;
            StandardInput = "socket";
            StandardOutput = "socket";
            StandardError = "journal";
            LoadCredential = [
              "credential-map:${authorizationMapFile}"
              "connect-token:${tokenFile}"
            ];
            TimeoutStartSec = "30s";
            DynamicUser = true;
            PrivateTmp = true;
            PrivateDevices = true;
            NoNewPrivileges = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            ProtectControlGroups = true;
            ProtectKernelTunables = true;
            ProtectKernelModules = true;
            ProtectKernelLogs = true;
            ProtectClock = true;
            LockPersonality = true;
            RestrictRealtime = true;
            RestrictSUIDSGID = true;
            RestrictNamespaces = true;
            RestrictAddressFamilies = [
              "AF_UNIX"
              "AF_INET"
              "AF_INET6"
            ];
            UMask = "0077";
          };
        };
      }
      consumerServices
    ];
  };
}
