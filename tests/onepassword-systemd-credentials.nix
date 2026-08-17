{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  replicaNames = [
    "debord"
    "dewey"
    "mali"
  ];
  virtualAddress = "192.0.2.22";
  tokenSopsFile = pkgs.writeText "onepassword-provider-test-secrets.yaml" "{}\n";
  testOp = pkgs.writeShellScriptBin "op" "exit 1";
  evaluate =
    {
      name,
      consumerService ? "example",
      reference ? "op://home-ops-prod/Example/password",
      bootstrapTokenFile ? null,
    }:
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        {
          options.site = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        }
        {
          networking.hostName = name;
          system.stateVersion = "26.05";
          site.net.mgmt.hosts4.onepassword-connect = [ virtualAddress ];
          modules.services.onepassword-systemd-credentials = {
            bootstrapTokenSopsFile = tokenSopsFile;
            inherit bootstrapTokenFile;
            package = testOp;
            consumers.${consumerService}.password = reference;
          };
          systemd.services = lib.optionalAttrs (consumerService == "example") {
            example.script = "true";
          };
        }
      ];
    }).config;
  configs = lib.genAttrs (replicaNames ++ [ "quine" ]) (name: evaluate { inherit name; });
  invalidReference =
    builtins.tryEval
      (evaluate {
        name = "debord";
        reference = "https://example.invalid/secret";
        bootstrapTokenFile = "/run/test-token";
      }).system.build.toplevel;
  storeBootstrapToken =
    builtins.tryEval
      (evaluate {
        name = "debord";
        bootstrapTokenFile = "/nix/store/example-token";
      }).system.build.toplevel;
  bootstrapConsumer =
    builtins.tryEval
      (evaluate {
        name = "debord";
        consumerService = "sops-install-secrets";
        bootstrapTokenFile = "/run/test-token";
      }).system.build.toplevel;
  authorizationMapFile = lib.removePrefix "credential-map:" (
    builtins.elemAt
      configs.debord.systemd.services."onepassword-credential-provider@".serviceConfig.LoadCredential
      0
  );
  expectedAuthorizationMap = builtins.toJSON {
    "example.service".password = "op://home-ops-prod/Example/password";
  };
  enabledHosts = lib.filter (
    name: configs.${name}.modules.services.onepassword-systemd-credentials.enable
  ) (builtins.attrNames configs);
  targetIsCorrect =
    name:
    let
      cfg = configs.${name};
      provider = cfg.modules.services.onepassword-systemd-credentials;
      socket = cfg.systemd.sockets.onepassword-credential-provider;
      helper = cfg.systemd.services."onepassword-credential-provider@";
      consumer = cfg.systemd.services.example;
      token = cfg.sops.secrets."onepassword-connect/token";
      hardening = lib.getAttrs [
        "DynamicUser"
        "PrivateTmp"
        "PrivateDevices"
        "NoNewPrivileges"
        "ProtectSystem"
        "ProtectHome"
        "ProtectControlGroups"
        "ProtectKernelTunables"
        "ProtectKernelModules"
        "ProtectKernelLogs"
        "ProtectClock"
        "LockPersonality"
        "RestrictRealtime"
        "RestrictSUIDSGID"
        "RestrictNamespaces"
        "RestrictAddressFamilies"
        "UMask"
        "TimeoutStartSec"
      ] helper.serviceConfig;
    in
    provider.connectHost == "http://${virtualAddress}:8080"
    && socket.wantedBy == [ "sockets.target" ]
    && socket.socketConfig.ListenStream == provider.socketPath
    && socket.socketConfig.Accept
    && socket.socketConfig.SocketMode == "0600"
    && helper.serviceConfig.StandardInput == "socket"
    && helper.serviceConfig.StandardOutput == "socket"
    && helper.serviceConfig.StandardError == "journal"
    && helper.wants == [ "network-online.target" ]
    && helper.after == [ "network-online.target" ]
    && helper.unitConfig.CollectMode == "inactive-or-failed"
    &&
      hardening == {
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
        TimeoutStartSec = "30s";
      }
    && lib.hasPrefix "credential-map:/nix/store/" (
      builtins.elemAt helper.serviceConfig.LoadCredential 0
    )
    && builtins.elemAt helper.serviceConfig.LoadCredential 1 == "connect-token:${token.path}"
    && token.sopsFile == tokenSopsFile
    && token.mode == "0400"
    && token.restartUnits == [ ]
    && builtins.elem "onepassword-credential-provider.socket" consumer.requires
    && builtins.elem "onepassword-credential-provider.socket" consumer.after
    && builtins.elem "password:${provider.socketPath}" consumer.serviceConfig.LoadCredential
    && builtins.elem "${pkgs.coreutils}/bin/test -s %d/password" consumer.serviceConfig.ExecStartPre;
in
assert enabledHosts == replicaNames;
assert lib.all targetIsCorrect replicaNames;
assert !invalidReference.success;
assert !storeBootstrapToken.success;
assert !bootstrapConsumer.success;
pkgs.runCommand "onepassword-systemd-credentials-evaluation" { } ''
  printf %s ${lib.escapeShellArg expectedAuthorizationMap} > expected-map.json
  cmp expected-map.json ${authorizationMapFile}
  touch $out
''
