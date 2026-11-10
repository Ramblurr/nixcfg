{ pkgs }:
let
  editor = pkgs.callPackage ../pkgs/dependency-mirror.nix { };
in
pkgs.testers.runNixOSTest {
  name = "code-search-mcp-private-service";
  nodes = {
    server = { config, lib, ... }: {
      imports = [
        ../modules/services/code-search-mcp.nix
        ../modules/services/neogrok.nix
      ];
      options.modules.services = {
        dependency-mirror = {
          enable = lib.mkEnableOption "fixture mirrors";
          domain = lib.mkOption { type = lib.types.str; };
          port = lib.mkOption {
            type = lib.types.port;
            default = 8084;
          };
          cgitPort = lib.mkOption {
            type = lib.types.port;
            default = 8085;
          };
          listenAddress = lib.mkOption {
            type = lib.types.str;
            default = "127.0.0.1";
          };
        };
        caddy.routes = lib.mkOption {
          type = lib.types.attrsOf (
            lib.types.submodule {
              options.handlerConfig = lib.mkOption {
                type = lib.types.lines;
                default = "";
              };
            }
          );
          default = { };
        };
      };
      config = {
        virtualisation.memorySize = 3072;
        virtualisation.cores = 2;
        networking.extraHosts = "127.0.0.1 src.example.test";
        networking.firewall.allowedTCPPorts = [ 80 ];
        modules.services.dependency-mirror = {
          enable = true;
          domain = "src.example.test";
        };
        modules.services.neogrok.enable = true;
        modules.services.code-search-mcp.enable = true;
        # Populate and index the fixture before the MCP startup refresh.
        systemd.services.code-search-mcp.wantedBy = lib.mkForce [ ];
        systemd.timers.neogrok-index.wantedBy = lib.mkForce [ ];
        users.groups.dependency-mirror = { };
        users.users.dependency-mirror = {
          isSystemUser = true;
          group = "dependency-mirror";
        };
        # The real inventory editor, so add_repos and sync_repos use its agent API.
        systemd.services.dependency-mirror = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            User = "dependency-mirror";
            Group = "dependency-mirror";
            ExecStart = "${lib.getExe editor} serve --state-dir /var/lib/dependency-mirror --bind 127.0.0.1 --port 8084 --cgit-url http://src.example.test/ --base-path /edit --public-origin http://src.example.test";
          };
        };
        systemd.tmpfiles.rules = [
          "d /var/lib/dependency-mirror 0750 dependency-mirror dependency-mirror -"
          "d /var/lib/dependency-mirror/repos 0750 dependency-mirror dependency-mirror -"
        ];
        services.caddy = {
          enable = true;
          extraConfig = ''
            http://src.example.test {
              ${config.modules.services.caddy.routes.dependency-mirror.handlerConfig}
              handle /edit/* {
                reverse_proxy 127.0.0.1:8084
              }
              handle {
                respond "cgit fixture"
              }
            }
          '';
        };
        environment.systemPackages = [
          pkgs.git
          pkgs.curl
          pkgs.python3
          pkgs.util-linux
        ];
      };
    };
    client = {
      environment.systemPackages = [ pkgs.curl ];
    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("multi-user.target")
    server.succeed("mkdir -p /tmp/work /var/lib/dependency-mirror/repos/github.com/test")
    server.succeed("git -C /tmp/work init -q -b main; git -C /tmp/work config user.name Fixture; git -C /tmp/work config user.email fixture@example.test")
    server.succeed("printf 'package main\\nfunc DemoSymbol() {}\\n' > /tmp/work/main.go; git -C /tmp/work add main.go; git -C /tmp/work commit -qm initial; git -C /tmp/work tag v1.0.0")
    server.succeed("git clone -q --bare /tmp/work /var/lib/dependency-mirror/repos/github.com/test/demo.git; chown -R dependency-mirror:dependency-mirror /var/lib/dependency-mirror")
    revision = server.succeed("git --git-dir=/var/lib/dependency-mirror/repos/github.com/test/demo.git rev-parse HEAD").strip()
    server.succeed("systemctl start neogrok-index.service")
    server.wait_for_unit("neogrok.service")
    server.wait_until_succeeds("curl -fsS -H 'Content-Type: application/json' -d '{}' http://127.0.0.1:3081/search/api/list | ${pkgs.python3}/bin/python -c 'import json,sys; assert len(json.load(sys.stdin)[\"results\"][\"repositories\"]) == 1'")
    server.succeed("systemctl start code-search-mcp.service")
    server.wait_for_open_port(3082)
    server.wait_for_unit("caddy.service")
    server.wait_for_unit("dependency-mirror.service")
    server.wait_for_open_port(8084)
    server.succeed("${pkgs.python3}/bin/python ${./code-search-mcp-client.py} http://src.example.test github.com/test/demo " + revision)
    # Existing route shapes still work. The search route uses the real frontend.
    server.succeed("curl -fsS http://src.example.test/ | grep 'cgit fixture'")
    server.succeed("curl -fsS http://src.example.test/edit/ >/dev/null")
    server.succeed("curl -fsSL http://src.example.test/search/ >/dev/null")
    # A different VM is outside the allowlist; forwarded headers cannot bypass it.
    client.succeed("test $(curl -s -o /dev/null -w '%{http_code}' -H 'Host: src.example.test' -H 'X-Forwarded-For: 127.0.0.1' http://server/mcp) = 403")
    client.fail("curl --connect-timeout 3 http://server:3082/mcp")
    pid = server.succeed("systemctl show code-search-mcp -p MainPID --value").strip()
    server.fail("nsenter -t " + pid + " -m -- touch /var/lib/dependency-mirror/repos/write-probe")
    server.succeed("test $(systemctl show code-search-mcp -p MemoryMax --value) = 1073741824")
    server.succeed("test $(systemctl show code-search-mcp -p TasksMax --value) = 128")
    server.succeed("grep -q 'NoNewPrivs:.*1' /proc/" + pid + "/status")
    server.succeed("test -f /var/lib/code-search-mcp/catalog.sqlite; systemctl is-active code-search-mcp neogrok zoekt caddy")
  '';
}
