{ inputs, pkgs }:
let
  connectToken = "fake-connect-token";
  connectUrl = "http://op-connect.example.ts.net:8080";
  clientKey = "client-key-fixture";
  # ts-plug opens its HTTP proxy only once its node is on a tailnet, which
  # the VM has none of. This fake Connect stands in for that proxy: it
  # serves on the proxy socket and takes requests in absolute form.
  fakeConnect = pkgs.writers.writePython3 "fake-connect" { flakeIgnore = [ "E501" ]; } ''
    import json
    import os
    import socketserver
    import sys
    from http.server import BaseHTTPRequestHandler

    VAULT = "vlt0000000000000000000000a"
    ITEMS = [
        {"id": "itm0000000000000000000000a", "title": "cliproxyapi",
         "fields": [{"id": "credential", "label": "credential", "value": "${clientKey}"},
                    {"id": "fld0000000000000000000000b", "label": "management-secret-unhashed",
                     "value": "management-fixture"},
                    {"id": "fld0000000000000000000000d", "label": "empty-field"},
                    {"id": "fld0000000000000000000000e", "label": "nbsp-field", "value": "\u00a0"}]},
        {"id": "itm0000000000000000000000c", "title": "OpenRouter",
         "fields": [{"id": "credential", "label": "credential", "value": "upstream-fixture"}]},
    ]


    def items():
        if os.path.exists("/run/fake-connect-no-client"):
            return [i for i in ITEMS if i["title"] != "cliproxyapi"]
        return ITEMS


    class Handler(BaseHTTPRequestHandler):
        def answer(self, status, body):
            data = json.dumps(body).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def log_message(self, *args):
            pass

        def do_GET(self):
            with open("/run/fake-connect/requests", "a") as log:
                log.write(self.path + "\n")
            if self.headers.get("Authorization") != "Bearer ${connectToken}":
                return self.answer(401, {"status": 401})
            if not self.path.startswith("${connectUrl}/"):
                return self.answer(502, {"status": 502})
            parts = self.path.removeprefix("${connectUrl}").split("?")[0].strip("/").split("/")
            if parts == ["v1", "vaults"]:
                return self.answer(200, [{"id": VAULT, "name": "moot-ward"}])
            if parts == ["v1", "vaults", VAULT, "items"]:
                return self.answer(200, [{"id": i["id"], "title": i["title"]} for i in items()])
            if len(parts) == 5 and parts[:4] == ["v1", "vaults", VAULT, "items"]:
                for i in items():
                    if i["id"] == parts[4]:
                        return self.answer(200, i)
            return self.answer(404, {"status": 404})


    socketserver.UnixStreamServer.allow_reuse_address = True
    server = socketserver.UnixStreamServer(sys.argv[1], Handler)
    os.chmod(sys.argv[1], 0o666)
    server.serve_forever()
  '';
in
pkgs.testers.runNixOSTest {
  name = "cliproxy-api-service";
  node.specialArgs = { inherit inputs; };
  nodes.machine =
    { ... }:
    {
      imports = [ ../modules/services/cliproxy-api.nix ];
      virtualisation.memorySize = 2048;
      environment.systemPackages = [ pkgs.curl ];
      modules.services.cliproxy-api = {
        enable = true;
        authKeyFile = "/run/cliproxy-test/authkey";
        onepassword = {
          url = connectUrl;
          tokenFile = "/run/cliproxy-test/token";
        };
        settings = {
          management.secret-key = "op://moot-ward/cliproxyapi/management-secret-unhashed";
          access.api-keys = [
            "op://moot-ward/cliproxyapi/credential"
            "op://moot-ward/cliproxyapi/empty-field"
            "op://moot-ward/cliproxyapi/nbsp-field"
            "op://moot-ward/missing/credential"
          ];
          api-keys.openai-compatibility = [
            {
              name = "openrouter";
              base-url = "https://openrouter.ai/api/v1";
              keys = [ { api-key = "op://moot-ward/OpenRouter/credential"; } ];
              models = [ { name = "test/upstream-model"; } ];
            }
          ];
        };
      };
      systemd.tmpfiles.rules = [
        "d /run/cliproxy-test 0700 root root -"
        "f+ /run/cliproxy-test/token 0600 root root - ${connectToken}"
        "f+ /run/cliproxy-test/authkey 0600 root root - tskey-auth-test"
      ];
      systemd.services.fake-connect = {
        requires = [ "cliproxy-api.service" ];
        after = [ "cliproxy-api.service" ];
        serviceConfig = {
          ExecStart = "${fakeConnect} /run/cliproxyapi/proxy.sock";
          RuntimeDirectory = "fake-connect";
        };
      };
      users.users.outsider = {
        isSystemUser = true;
        group = "outsider";
      };
      users.groups.outsider = { };
    };

  testScript = ''
    import json

    API = "http://127.0.0.1:8317"
    CONFIG = "/run/cliproxyapi/config.yaml"


    def status(path, key=None):
        auth = f"-H 'Authorization: Bearer {key}'" if key else ""
        return machine.succeed(f"curl -s -o /dev/null -w '%{{http_code}}' {auth} {API}{path}").strip()


    machine.wait_for_unit("cliproxy-api.service")
    machine.wait_until_succeeds("test -d /run/cliproxyapi")
    machine.succeed("systemctl start fake-connect.service")
    machine.wait_for_open_port(8317, "127.0.0.1")

    with subtest("op:// references resolve through the proxy socket with the loaded token"):
        requests = machine.succeed("cat /run/fake-connect/requests").split()
        assert requests and all(r.startswith("${connectUrl}/v1/") for r in requests), requests
        rendered = json.loads(machine.succeed(f"cat {CONFIG}"))
        assert rendered["access"]["api-keys"] == ["${clientKey}"], rendered["access"]
        compat = rendered["api-keys"]["openai-compatibility"][0]
        assert compat["keys"] == [{"api-key": "upstream-fixture"}], compat
        assert rendered["server"] == {"host": "127.0.0.1", "port": 8317}, rendered["server"]
        machine.succeed("journalctl -u cliproxy-api | grep -q 'Skipping credential: Cannot resolve op://moot-ward/missing/credential'")

    with subtest("CLIProxyAPI serves on loopback and requires a client key"):
        assert status("/v1/models", "${clientKey}") == "200"
        assert status("/v1/models") == "401"
        assert status("/v1/models", "wrong-key") == "401"
        assert status("/v1/models", "nil") == "401"
        models = machine.succeed(f"curl -sf -H 'Authorization: Bearer ${clientKey}' {API}/v1/models")
        assert "test/upstream-model" in models, models
        machine.fail("ss -Hltn 'sport = :8317' | grep -v 127.0.0.1")

    with subtest("both processes run hardened as cliproxyapi"):
        machine.succeed("pgrep -u cliproxyapi -x ts-plug")
        machine.succeed("pgrep -u cliproxyapi -x cli-proxy-api")
        machine.succeed("systemctl show -p NoNewPrivileges --value cliproxy-api | grep -qx yes")
        machine.succeed("systemctl show -p ProtectSystem --value cliproxy-api | grep -qx strict")

    with subtest("OAuth auths and state persist in the state directory"):
        machine.succeed("test -d /var/lib/cliproxyapi/auths")
        machine.succeed("test -e /var/lib/cliproxyapi/kv.db")
        machine.succeed("test -d /var/lib/cliproxyapi-tailscale")
        assert machine.succeed("stat -c '%a %U' /var/lib/cliproxyapi").strip() == "700 cliproxyapi"
        machine.succeed("touch /var/lib/cliproxyapi/auths/marker.json && chown cliproxyapi /var/lib/cliproxyapi/auths/marker.json")
        machine.succeed("systemctl restart cliproxy-api.service")
        machine.wait_for_open_port(8317, "127.0.0.1")
        machine.succeed("test -e /var/lib/cliproxyapi/auths/marker.json")

    with subtest("no other user reaches the config, the proxy or the credentials"):
        assert machine.succeed(f"stat -c '%a %U' {CONFIG}").strip() == "600 cliproxyapi"
        assert machine.succeed("stat -c '%a %U' /run/cliproxyapi").strip() == "700 cliproxyapi"
        machine.fail(f"runuser -u outsider -- cat {CONFIG}")
        machine.fail("runuser -u outsider -- curl -sf --unix-socket /run/cliproxyapi/proxy.sock http://localhost/")
        machine.fail("runuser -u outsider -- cat /run/credentials/cliproxy-api.service/op-connect-token")
        machine.fail("runuser -u outsider -- cat /run/credentials/cliproxy-api.service/ts-authkey")
        pid = machine.succeed("pgrep -u cliproxyapi -x cli-proxy-api").strip()
        machine.fail(f"grep -qa '${connectToken}\\|tskey-auth-test' /proc/{pid}/environ")

    with subtest("the service fails closed when no client key resolves"):
        machine.succeed("touch /run/fake-connect-no-client")
        machine.succeed("systemctl restart cliproxy-api.service")
        machine.wait_until_succeeds("journalctl -u cliproxy-api | grep -q 'Config generation failed: No client API key resolved'")
        machine.fail(f"curl -s {API}/v1/models")
        machine.fail(f"test -e {CONFIG}")
  '';
}
