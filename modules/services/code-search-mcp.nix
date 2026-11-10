{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.code-search-mcp;
  mirrors = "/var/lib/dependency-mirror/repos";
  stateDir = "/var/lib/code-search-mcp";
in
{
  options.modules.services.code-search-mcp = {
    enable = lib.mkEnableOption "MCP access to dependency mirrors, with inventory add and sync tools";
    port = lib.mkOption {
      type = lib.types.port;
      default = 3082;
      description = "Loopback MCP listener port";
    };
    allowedRemoteIPs = lib.mkOption {
      type = lib.types.listOf lib.types.nonEmptyStr;
      default = [
        "127.0.0.1/32"
        "::1/128"
        "100.64.0.0/10"
        "fd7a:115c:a1e0::/48"
      ];
      description = "Direct client networks permitted on /mcp and /mcp/all; no forwarded-IP trust or agent token";
    };
    disabledTools = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "[a-z_]+");
      default = [ ];
      example = [ "sync_repos" ];
      description = "MCP tool names removed from every endpoint; unknown names stop the service at start";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          config.modules.services.dependency-mirror.enable && config.modules.services.neogrok.enable;
        message = "Code-search MCP requires dependency mirrors and Neogrok.";
      }
      {
        assertion = cfg.allowedRemoteIPs != [ ];
        message = "Code-search MCP requires an explicit private-network allowlist.";
      }
      {
        assertion =
          !(builtins.elem cfg.port [
            config.modules.services.neogrok.port
            config.modules.services.neogrok.zoektPort
            config.modules.services.dependency-mirror.port
            config.modules.services.dependency-mirror.cgitPort
          ]);
        message = "Code-search MCP needs a distinct loopback port.";
      }
    ];

    users.users.code-search-mcp = {
      isSystemUser = true;
      group = "code-search-mcp";
    };
    users.groups.code-search-mcp = { };

    modules.services.caddy.routes.dependency-mirror.handlerConfig = lib.mkBefore ''
      @code_search_mcp path /mcp /mcp/*
      handle @code_search_mcp {
        @code_search_clients remote_ip ${lib.concatStringsSep " " cfg.allowedRemoteIPs}
        handle @code_search_clients {
          request_body {
            max_size 65536
          }
          reverse_proxy 127.0.0.1:${toString cfg.port}
        }
        respond 403
      }
    '';

    systemd.services.code-search-mcp = {
      description = "Dependency code-search MCP";
      wantedBy = [ "multi-user.target" ];
      wants = [ "neogrok.service" ];
      after = [
        "neogrok.service"
        "systemd-tmpfiles-setup.service"
      ];
      unitConfig.RequiresMountsFor = [ mirrors ];
      environment = {
        PORT = toString cfg.port;
        CODE_SEARCH_ORIGIN = "https://${config.modules.services.dependency-mirror.domain}";
        NEOGROK_URL = "http://127.0.0.1:${toString config.modules.services.neogrok.port}/search";
        MIRROR_ROOT = mirrors;
        CODE_SEARCH_DISABLED_TOOLS = lib.concatStringsSep "," cfg.disabledTools;
        # add_repos and sync_repos use the editor's agent API over loopback.
        EDITOR_URL = "http://${config.modules.services.dependency-mirror.listenAddress}:${toString config.modules.services.dependency-mirror.port}/edit";
        HOME = stateDir;
      };
      serviceConfig = {
        ExecStart = lib.getExe pkgs.code-search-mcp;
        User = "code-search-mcp";
        Group = "code-search-mcp";
        SupplementaryGroups = [ "dependency-mirror" ];
        StateDirectory = "code-search-mcp";
        StateDirectoryMode = "0700";
        WorkingDirectory = stateDir;
        UMask = "0077";
        Restart = "on-failure";
        RestartSec = 5;
        TimeoutStopSec = 20;
        KillMode = "control-group";
        MemoryMax = "1G";
        CPUQuota = "200%";
        TasksMax = 128;
        NoNewPrivileges = true;
        CapabilityBoundingSet = [ ];
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadOnlyPaths = [ "/var/lib/dependency-mirror" ];
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        IPAddressDeny = "any";
        IPAddressAllow = "localhost";
      };
    };
  };
}
