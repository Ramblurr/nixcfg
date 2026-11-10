{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  system =
    modules:
    (lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      specialArgs = { inherit inputs; };
      modules = [
        ../modules/services/bb.nix
        ../modules/services/bb-isolation
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "25.05";
          users.users.developer = {
            isNormalUser = true;
            home = "/home/developer";
          };
        }
      ]
      ++ modules;
    }).config;
  serverModule = {
    services.bb = {
      enable = true;
      user = "developer";
    };
    services.bbIsolation.gateway.enable = true;
  };
  workerModule.services.bbIsolation.egress = {
    enable = true;
    user = "developer";
    gatewayAddress = "100.64.1.2";
  };
  disabled = system [ ];
  server = system [ serverModule ];
  worker = system [ workerModule ];
  combined = system [
    serverModule
    workerModule
  ];
  invalid = system [
    serverModule
    { services.bb.bindHost = "0.0.0.0"; }
  ];
  tests = {
    disabled =
      !(disabled.systemd.services ? bb-worker-egress) && !(disabled.systemd.services ? bb-worker-gateway);
    serverOnly =
      !(server.systemd.services ? bb-worker-egress) && server.systemd.services ? bb-worker-gateway;
    workerOnly =
      !(worker.systemd.services ? bb-worker-gateway)
      && !(worker.systemd.services ? bb)
      && worker.systemd.services ? bb-worker-egress;
    combined =
      combined.systemd.services ? bb-worker-egress && combined.systemd.services ? bb-worker-gateway;
    stateInConfig =
      worker.services.bbIsolation.egress.tokenFile
      == "/home/developer/.config/bb/isolation/interceptor-token";
    tokenRuntimeOnly = lib.hasInfix "--token-file" worker.systemd.services.bb-worker-egress.serviceConfig.ExecStart;
    gatewayPrivate =
      server.systemd.services.bb-worker-gateway.serviceConfig.DynamicUser
      && !(lib.elem 38895 server.networking.firewall.allowedTCPPorts);
    separateIngress = lib.hasInfix "--https=8445 http://127.0.0.1:38895" server.systemd.services.bb-worker-tailscale-serve.serviceConfig.ExecStart;
    rejectsExposedBackend = lib.any (
      a: !a.assertion && lib.hasPrefix "bbIsolation gateway requires" a.message
    ) invalid.assertions;
  };
in
assert lib.assertMsg (lib.all (x: x) (lib.attrValues tests))
  "bb isolation module tests failed: ${
    lib.concatStringsSep ", " (lib.attrNames (lib.filterAttrs (_: v: !v) tests))
  }";
pkgs.runCommand "bb-isolation-tests"
  {
    nativeBuildInputs = [
      pkgs.python3
      pkgs.caddy
    ];
  }
  ''
    export PYTHONDONTWRITEBYTECODE=1
    export PYTHONPATH=${../modules/services/bb-isolation}
    export GATEWAY_TEMPLATE=${../modules/services/bb-isolation/Caddyfile.in}
    export CADDY=${lib.getExe pkgs.caddy}
    python ${./bb-isolation.py} -v
    echo ${lib.escapeShellArg (builtins.toJSON tests)} > "$out"
  ''
