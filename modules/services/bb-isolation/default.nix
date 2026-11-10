{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.bbIsolation;
  gatewayConfig = pkgs.writeText "bb-worker-gateway.Caddyfile" (
    lib.replaceStrings
      [ "@listenPort@" "@backendPort@" ]
      [ (toString cfg.gateway.listenPort) (toString cfg.gateway.backendPort) ]
      (builtins.readFile ./Caddyfile.in)
  );
  policyArgs = [
    "--token-file"
    cfg.egress.tokenFile
    "--gateway-ip"
    cfg.egress.gatewayAddress
    "--gateway-port"
    (toString cfg.egress.gatewayPort)
    "--port"
    (toString cfg.egress.listenPort)
  ]
  ++ lib.concatMap (cidr: [
    "--deny-cidr"
    cidr
  ]) cfg.egress.denyCidrs
  ++ lib.concatMap (service: [
    "--service"
    "${service.address}:${toString service.port}=${service.target}:${toString service.targetPort}"
  ]) cfg.egress.hostServices;
  # Read by Sandbox (`:mediation` in its config.edn). Paths and names only, no token.
  mediationPolicy = {
    broker = "127.0.0.1:${toString cfg.egress.listenPort}";
    inherit (cfg.egress) tokenFile;
    services = map (service: {
      inherit (service)
        name
        address
        port
        hostname
        ;
    }) cfg.egress.hostServices;
  };
  hardening = {
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    RestrictSUIDSGID = true;
    LockPersonality = true;
    CapabilityBoundingSet = "";
    RestrictAddressFamilies = [
      "AF_UNIX"
      "AF_INET"
      "AF_INET6"
    ];
    Restart = "on-failure";
    RestartSec = 2;
  };
in
{
  options.services.bbIsolation = {
    gateway = {
      enable = lib.mkEnableOption "the restricted bb worker ingress";
      listenPort = lib.mkOption {
        type = lib.types.port;
        default = 16895;
        description = "Loopback HTTP listener, behind Tailscale HTTPS.";
      };
      backendPort = lib.mkOption {
        type = lib.types.port;
        default = config.services.bb.port;
        description = "Local bb backend port. Its unrestricted API must stay on loopback.";
      };
      httpsPort = lib.mkOption {
        type = lib.types.port;
        default = 8445;
        description = "Tailscale worker HTTPS port; separate from the human client port.";
      };
    };
    egress = {
      enable = lib.mkEnableOption "the host-side SmolVM bb egress decider";
      user = lib.mkOption {
        type = lib.types.str;
        description = "Trusted host user running SmolVM. Never mount this user's state in guests.";
      };
      stateDir = lib.mkOption {
        type = lib.types.str;
        default = "${config.users.users.${cfg.egress.user}.home}/.config/bb/isolation";
        description = "Private host-side policy state, outside every guest.";
      };
      tokenFile = lib.mkOption {
        type = lib.types.str;
        default = "${cfg.egress.stateDir}/interceptor-token";
        description = "Generated 0600 runtime token file. Never copy into a guest or the Nix store.";
      };
      gatewayAddress = lib.mkOption {
        type = lib.types.str;
        description = "Exact Tailscale IP of the bb server host, local or remote.";
      };
      gatewayPort = lib.mkOption {
        type = lib.types.port;
        default = 8445;
        description = "Only this port on gatewayAddress is allowed.";
      };
      listenPort = lib.mkOption {
        type = lib.types.port;
        default = 16896;
        description = "Loopback SMOLMEG2 listener, supplied at every mediated VM start.";
      };
      gatewayHostname = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "TLS name of the worker gateway. Guests resolve it to the gateway placeholder.";
      };
      hostServices = lib.mkOption {
        type = lib.types.listOf (
          lib.types.submodule {
            options = {
              name = lib.mkOption {
                type = lib.types.strMatching "[a-z][a-z0-9-]{0,62}";
                description = "Name a sandbox uses to select this service.";
              };
              address = lib.mkOption {
                type = lib.types.strMatching "192\\.0\\.2\\.([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-4])";
                description = "Placeholder guest address in 192.0.2.0/24 (TEST-NET-1, never routed).";
              };
              port = lib.mkOption {
                type = lib.types.port;
                description = "Placeholder guest port.";
              };
              target = lib.mkOption {
                type = lib.types.str;
                description = "Host-side destination address the broker connects to.";
              };
              targetPort = lib.mkOption {
                type = lib.types.port;
                description = "Host-side destination port.";
              };
              hostname = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "Name guests resolve to the placeholder; keep the real TLS name.";
              };
            };
          }
        );
        default = [
          {
            name = "bb-worker";
            address = "192.0.2.1";
            port = cfg.egress.gatewayPort;
            target = cfg.egress.gatewayAddress;
            targetPort = cfg.egress.gatewayPort;
            hostname = cfg.egress.gatewayHostname;
          }
        ];
        defaultText = lib.literalExpression "[ { name = \"bb-worker\"; address = \"192.0.2.1\"; port = gatewayPort; target = gatewayAddress; targetPort = gatewayPort; hostname = gatewayHostname; } ]";
        description = ''
          Host services that mediated VMs can reach through placeholder addresses.
          A machine sends placeholder traffic to the broker with a per-VM redirect
          rule, and the broker connects to the target. This list is the complete
          set; a sandbox selects entries from it by name and cannot add targets.
          The SmolVM strict floor stays in place for every real private address.
        '';
      };
      denyCidrs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Additional public destinations to block, including any public addresses
          exposing the unrestricted bb API. Private, link-local, loopback and
          Tailscale destinations are already denied except gatewayAddress:gatewayPort.
          Other global unicast destinations allow TCP 80/443 only.
        '';
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.gateway.enable {
      assertions = [
        {
          assertion =
            config.services.bb.enable
            && config.services.bb.role == "server"
            && config.services.bb.bindHost == "127.0.0.1"
            && !config.services.bb.openFirewall;
          message = "bbIsolation gateway requires a loopback-only local bb server.";
        }
        {
          assertion = cfg.gateway.listenPort != cfg.gateway.backendPort;
          message = "bbIsolation gateway and backend ports must differ.";
        }
      ];
      services.tailscale.enable = true;
      systemd.services.bb-worker-gateway = {
        description = "bb restricted worker ingress";
        wantedBy = [ "multi-user.target" ];
        after = [ "bb.service" ];
        wants = [ "bb.service" ];
        environment = {
          XDG_DATA_HOME = "/run/bb-worker-gateway";
          XDG_CONFIG_HOME = "/run/bb-worker-gateway";
        };
        serviceConfig = hardening // {
          DynamicUser = true;
          ProtectHome = true;
          RuntimeDirectory = "bb-worker-gateway";
          ExecStart = "${lib.getExe pkgs.caddy} run --config ${gatewayConfig} --adapter caddyfile";
        };
      };
      # Foreground Serve owns just this port. The pinned Tailscale module has
      # no services.tailscale.serve.services option.
      systemd.services.bb-worker-tailscale-serve = {
        description = "bb worker Tailscale HTTPS listener";
        wantedBy = [ "multi-user.target" ];
        requires = [ "tailscaled.service" ];
        wants = [ "bb-worker-gateway.service" ];
        after = [
          "tailscaled.service"
          "bb-worker-gateway.service"
        ];
        serviceConfig = {
          ExecStart = "${lib.getExe config.services.tailscale.package} serve --https=${toString cfg.gateway.httpsPort} http://127.0.0.1:${toString cfg.gateway.listenPort}";
          Restart = "on-failure";
          RestartSec = 5;
        };
      };
    })
    (lib.mkIf cfg.egress.enable {
      assertions = [
        {
          assertion = lib.hasPrefix "${cfg.egress.stateDir}/" cfg.egress.tokenFile;
          message = "bbIsolation tokenFile must be inside stateDir.";
        }
        {
          assertion =
            let
              names = map (service: service.name) cfg.egress.hostServices;
              placeholders = map (service: "${service.address}:${toString service.port}") cfg.egress.hostServices;
            in
            lib.length names == lib.length (lib.unique names)
            && lib.length placeholders == lib.length (lib.unique placeholders);
          message = "bbIsolation hostServices names and placeholder address:port pairs must be unique.";
        }
      ];
      environment.etc."bb-isolation/mediation.json".text = builtins.toJSON mediationPolicy;
      systemd.tmpfiles.settings."10-bb-isolation" =
        let
          home = config.users.users.${cfg.egress.user}.home;
          directory = {
            mode = "0700";
            user = cfg.egress.user;
            group = config.users.users.${cfg.egress.user}.group;
          };
        in
        {
          ${cfg.egress.stateDir}.d = directory;
        }
        // lib.optionalAttrs (lib.hasPrefix "${home}/.config/bb/" cfg.egress.stateDir) {
          "${home}/.config/bb".d = directory;
        };
      systemd.services.bb-worker-egress = {
        description = "SmolVM bb worker egress policy";
        wantedBy = [ "multi-user.target" ];
        preStart = ''
          ${lib.getExe pkgs.python3} - <<'PY'
          import os, secrets
          path = ${builtins.toJSON cfg.egress.tokenFile}
          try:
              fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
          except FileExistsError:
              pass
          else:
              with os.fdopen(fd, 'w') as stream:
                  stream.write(secrets.token_hex(32) + '\n')
          PY
        '';
        serviceConfig = hardening // {
          User = cfg.egress.user;
          Group = config.users.users.${cfg.egress.user}.group;
          ProtectHome = "read-only";
          ReadWritePaths = [ cfg.egress.stateDir ];
          UMask = "0077";
          ExecStart = "${lib.getExe pkgs.python3} ${./egress.py} ${lib.escapeShellArgs policyArgs}";
        };
      };
    })
  ];
}
