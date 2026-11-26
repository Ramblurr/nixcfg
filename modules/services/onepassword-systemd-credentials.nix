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
  tokenFile = cfg.bootstrapTokenFile;
  microvmConsumers = lib.mapAttrs' (
    guest: credentials: lib.nameValuePair "microvm-secrets-${guest}" credentials
  ) cfg.microvmSecrets;
  consumers = cfg.consumers // microvmConsumers;
  microvmSecretServices = lib.mapAttrs' (
    guest: credentials:
    let
      directory = "/run/microvms/secrets/${guest}";
      credentialNames = builtins.attrNames credentials;
    in
    lib.nameValuePair "microvm-secrets-${guest}" {
      description = "Materialize 1Password credentials for MicroVM '${guest}'";
      before = [
        "microvm-virtiofsd@${guest}.service"
        "microvm@${guest}.service"
      ];
      partOf = [ "microvm@${guest}.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "root";
        Group = "root";
        UMask = "0077";
        ExecStopPost = "${pkgs.coreutils}/bin/rm -rf ${lib.escapeShellArg directory}";
      };
      script = ''
        set -eu
        directory=${lib.escapeShellArg directory}
        ${pkgs.coreutils}/bin/install -d -m 0700 -o root -g root "$directory"
        ${pkgs.coreutils}/bin/rm -f "$directory"/*
        trap '${pkgs.coreutils}/bin/rm -f "$directory"/*.new' EXIT
        ${lib.concatMapStringsSep "\n" (credential: ''
          ${pkgs.coreutils}/bin/install -m 0400 "$CREDENTIALS_DIRECTORY/${credential}" ${lib.escapeShellArg "${directory}/${credential}.new"}
        '') credentialNames}
        ${lib.concatMapStringsSep "\n" (credential: ''
          ${pkgs.coreutils}/bin/mv -f ${lib.escapeShellArg "${directory}/${credential}.new"} ${lib.escapeShellArg "${directory}/${credential}"}
        '') credentialNames}
      '';
    }
  ) cfg.microvmSecrets;
  microvmSecretDependencies = lib.foldl' (
    result: guest:
    let
      directory = "/run/microvms/secrets/${guest}";
      secrets = "microvm-secrets-${guest}.service";
    in
    result
    // {
      "microvm-virtiofsd@${guest}" = {
        wants = [ secrets ];
        after = [ secrets ];
        unitConfig.AssertPathIsDirectory = directory;
      };
      "microvm@${guest}" = {
        wants = [ secrets ];
        after = [ secrets ];
        partOf = [ secrets ];
        serviceConfig.ExecStartPre = "${pkgs.coreutils}/bin/test -d ${directory}";
      };
    }
  ) { } (builtins.attrNames cfg.microvmSecrets);
  authorizationMap = lib.mapAttrs' (
    service: credentials: lib.nameValuePair "${service}.service" credentials
  ) consumers;
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
      import time
      from urllib.parse import urlsplit

      LOOKUP_RETRY_SECONDS = 15
      LOOKUP_TIMEOUT_SECONDS = 5
      MAX_CREDENTIAL_SIZE = 1024 * 1024
      CONNECT_HOSTS = json.loads(${builtins.toJSON (builtins.toJSON cfg.connectHosts)})


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
              "HOME": "/tmp",
              "XDG_CONFIG_HOME": "/tmp/op-config",
              "OP_CACHE": "false",
              "OP_CONNECT_TOKEN": token,
          }
      )


      def listener_uids(port):
          uids = []
          for table in ("/proc/net/tcp", "/proc/net/tcp6"):
              try:
                  lines = Path(table).read_text(encoding="ascii").splitlines()[1:]
              except FileNotFoundError:
                  continue
              for line in lines:
                  fields = line.split()
                  if fields[3] == "0A" and int(fields[1].rsplit(":", 1)[1], 16) == port:
                      uids.append(int(fields[7]))
          return uids


      def trusted(connect_host):
          expected_uid = connect_host["listenerUid"]
          if expected_uid is None:
              return True
          url = urlsplit(connect_host["url"])
          uids = listener_uids(url.port or (443 if url.scheme == "https" else 80))
          return bool(uids) and all(uid == expected_uid for uid in uids)


      def lookup(deadline):
          for connect_host in CONNECT_HOSTS:
              if not trusted(connect_host):
                  continue
              remaining = deadline - time.monotonic()
              if remaining <= 0:
                  return None
              try:
                  result = subprocess.run(
                      [${builtins.toJSON (lib.getExe cfg.package)}, "read", "--no-newline", reference],
                      stdin=subprocess.DEVNULL,
                      stdout=subprocess.PIPE,
                      stderr=subprocess.DEVNULL,
                      env=environment | {"OP_CONNECT_HOST": connect_host["url"]},
                      check=False,
                      timeout=min(LOOKUP_TIMEOUT_SECONDS, remaining),
                  )
              except OSError:
                  fail(f"lookup failed for {unit_name}/{credential_id}")
              except subprocess.TimeoutExpired:
                  continue
              if result.returncode == 0:
                  return result.stdout
          return None


      deadline = time.monotonic() + LOOKUP_RETRY_SECONDS
      while (value := lookup(deadline)) is None:
          remaining = deadline - time.monotonic()
          if remaining <= 0:
              fail(f"lookup failed for {unit_name}/{credential_id}")
          time.sleep(min(1, remaining))
      if not value:
          fail(f"lookup returned an empty value for {unit_name}/{credential_id}")
      if len(value) > MAX_CREDENTIAL_SIZE:
          fail(f"lookup exceeded the credential size limit for {unit_name}/{credential_id}")

      sys.stdout.buffer.write(value)
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
  }) consumers;
in
{
  options.modules.services.onepassword-systemd-credentials = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = builtins.elem config.networking.hostName targetHosts;
      description = "Whether to provide 1Password-backed credentials to system services.";
    };

    connectHosts = lib.mkOption {
      type = lib.types.nonEmptyListOf (
        lib.types.submodule {
          options = {
            url = lib.mkOption {
              type = lib.types.str;
              description = "Connect endpoint URL.";
            };
            listenerUid = lib.mkOption {
              type = lib.types.nullOr lib.types.int;
              default = null;
              description = ''
                When set, the endpoint is used only while every socket listening
                on its port belongs to this uid, so no other local user can
                receive the bootstrap token by binding the port first.
              '';
            };
          };
        }
      );
      default =
        lib.optional (config.modules.services.onepassword-connect.enable or false) {
          url = "http://127.0.0.1:8080";
          listenerUid = config.modules.services.onepassword-connect.user.uid;
        }
        ++ [ { url = "http://${builtins.head config.site.net.mgmt.hosts4.onepassword-connect}:8080"; } ];
      description = ''
        Connect endpoints used for credential lookups, most preferred first.
        Each lookup attempt tries them in order and uses the first that answers.
        Defaults to the host's own Connect replica, when it runs one, then the
        shared availability endpoint.
      '';
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

    bootstrapTokenFile = lib.mkOption {
      type = lib.types.str;
      default = "/persist/etc/op-connect-token";
      description = ''
        Host-local Connect token file, placed over SSH outside Nix and replaced
        on rotation. Each lookup reads it afresh, so a new token needs no deploy.
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

    microvmSecrets = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf (lib.types.strMatching "^op://.+"));
      default = { };
      example = {
        example-guest.API_TOKEN = "op://home-ops-prod/Example/token";
      };
      description = ''
        1Password references to materialize as volatile files for MicroVM guests.
        Each guest gets a read-only virtiofs source directory at
        /run/microvms/secrets/<guest>; each attribute name is the filename.
        The corresponding guest must opt into modules.microvm-guest.hostSecrets.
      '';
    };

    creds = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
      readOnly = true;
      default = lib.mapAttrs (
        service: credentials:
        lib.mapAttrs (credentialId: _: "/run/credentials/${service}.service/${credentialId}") credentials
      ) consumers;
      description = "Runtime systemd credential paths generated for configured consumers.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasPrefix "/" cfg.socketPath;
        message = "The 1Password credential provider socket path must be absolute.";
      }
      {
        assertion = lib.hasPrefix "/" cfg.bootstrapTokenFile;
        message = "The external 1Password provider bootstrap token path must be absolute.";
      }
      {
        assertion = !lib.hasPrefix builtins.storeDir cfg.bootstrapTokenFile;
        message = "The 1Password provider bootstrap token must not come from the Nix store.";
      }
      {
        assertion = lib.all (service: !(lib.hasSuffix ".service" service)) (builtins.attrNames consumers);
        message = "1Password credential consumer names must omit the .service suffix.";
      }
      {
        assertion = lib.intersectLists bootstrapServiceNames (builtins.attrNames consumers) == [ ];
        message = "Bootstrap-critical services cannot use the 1Password credential provider.";
      }
      {
        assertion = lib.all (
          credentials:
          lib.all (credentialId: builtins.match "[A-Za-z0-9_.-]+" credentialId != null) (
            builtins.attrNames credentials
          )
        ) (builtins.attrValues consumers);
        message = "1Password credential IDs may contain only letters, digits, dot, underscore, and hyphen.";
      }
      {
        assertion =
          lib.intersectLists (builtins.attrNames cfg.consumers) (builtins.attrNames microvmConsumers) == [ ];
        message = "MicroVM 1Password secret service names must not overlap regular consumers.";
      }
      {
        assertion = lib.all (guest: builtins.match "[A-Za-z0-9][A-Za-z0-9_.-]*" guest != null) (
          builtins.attrNames cfg.microvmSecrets
        );
        message = "MicroVM 1Password secret guest names contain invalid characters.";
      }
      {
        assertion = lib.all (guest: cfg.microvmSecrets.${guest} != { }) (
          builtins.attrNames cfg.microvmSecrets
        );
        message = "Each MicroVM 1Password secret guest must define at least one file.";
      }
    ];

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

    systemd.tmpfiles.rules = lib.optionals (cfg.microvmSecrets != { }) [
      "d /run/microvms 0755 root root -"
      "d /run/microvms/secrets 0755 root root -"
    ];

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
      microvmSecretServices
      microvmSecretDependencies
    ];
  };
}
