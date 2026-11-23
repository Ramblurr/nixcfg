{ inputs, pkgs }:
let
  clientKey = "client-key-fixture";
  upstreamKey = "upstream-fixture";
  dir = "/run/cliproxy-test";
  nbsp = builtins.fromJSON ''"\u00a0"'';
  separators = builtins.fromJSON ''"\u2028\u2029"'';
  references = {
    "op://mootward/cliproxyapi/credential" = "client-key";
    "op://mootward/cliproxyapi/management-secret-unhashed" = "management";
    "op://mootward/cliproxyapi/empty-field" = "empty";
    "op://mootward/cliproxyapi/nbsp-field" = "nbsp";
    "op://mootward/cliproxyapi/sep-field" = "sep";
    "op://mootward/OpenRouter/credential" = "openrouter";
  };
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
        authKeyFile = "${dir}/authkey";
        credentials = builtins.mapAttrs (_: name: "${dir}/${name}") references;
        settings = {
          management.secret-key = "op://mootward/cliproxyapi/management-secret-unhashed";
          access.api-keys = [
            "op://mootward/cliproxyapi/credential"
            "op://mootward/cliproxyapi/empty-field"
            "op://mootward/cliproxyapi/nbsp-field"
            "op://mootward/cliproxyapi/sep-field"
          ];
          api-keys.openai-compatibility = [
            {
              name = "openrouter";
              base-url = "https://openrouter.ai/api/v1";
              keys = [ { api-key = "op://mootward/OpenRouter/credential"; } ];
              models = [ { name = "test/upstream-model"; } ];
            }
          ];
        };
      };
      systemd.tmpfiles.rules = [
        "d ${dir} 0700 root root -"
        "f+ ${dir}/authkey 0600 root root - tskey-auth-test"
        "f+ ${dir}/client-key 0600 root root - ${clientKey}"
        "f+ ${dir}/management 0600 root root - management-fixture"
        "f+ ${dir}/empty 0600 root root -"
        "f+ ${dir}/nbsp 0600 root root - ${nbsp}"
        "f+ ${dir}/sep 0600 root root - ${separators}"
        "f+ ${dir}/openrouter 0600 root root - ${upstreamKey}"
      ];
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
    machine.wait_for_open_port(8317, "127.0.0.1", timeout=30)

    with subtest("the blank fixtures hold Unicode whitespace"):
        assert machine.succeed("od -An -tx1 ${dir}/nbsp").split() == ["c2", "a0"]
        assert machine.succeed("od -An -tx1 ${dir}/sep").split() == ["e2", "80", "a8", "e2", "80", "a9"]
        assert machine.succeed("stat -c %s ${dir}/empty").strip() == "0"

    with subtest("op:// references render from the loaded credentials"):
        rendered = json.loads(machine.succeed(f"cat {CONFIG}"))
        assert rendered["access"]["api-keys"] == ["${clientKey}"], rendered["access"]
        # CLIProxyAPI replaces the plaintext management secret with its bcrypt hash.
        assert rendered["management"]["secret-key"].startswith("$2"), rendered["management"]
        compat = rendered["api-keys"]["openai-compatibility"][0]
        assert compat["keys"] == [{"api-key": "${upstreamKey}"}], compat
        assert rendered["server"] == {"host": "127.0.0.1", "port": 8317}, rendered["server"]
        for field in ["empty-field", "nbsp-field", "sep-field"]:
            machine.succeed(f"journalctl -u cliproxy-api | grep -q 'Skipping credential: Empty credential: op://mootward/cliproxyapi/{field}'")

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
        machine.wait_for_open_port(8317, "127.0.0.1", timeout=30)
        machine.succeed("test -e /var/lib/cliproxyapi/auths/marker.json")

    with subtest("no other user reaches the config or the credentials"):
        assert machine.succeed(f"stat -c '%a %U' {CONFIG}").strip() == "600 cliproxyapi"
        assert machine.succeed("stat -c '%a %U' /run/cliproxyapi").strip() == "700 cliproxyapi"
        machine.fail(f"runuser -u outsider -- cat {CONFIG}")
        credentials = machine.succeed("ls /run/credentials/cliproxy-api.service").split()
        assert len(credentials) == 7, credentials
        for credential in credentials:
            machine.fail(f"runuser -u outsider -- cat /run/credentials/cliproxy-api.service/{credential}")
        pid = machine.succeed("pgrep -u cliproxyapi -x cli-proxy-api").strip()
        machine.fail(f"grep -qa '${clientKey}\\|${upstreamKey}\\|management-fixture\\|tskey-auth-test' /proc/{pid}/environ")

    with subtest("the service fails closed when only blank client keys remain"):
        machine.succeed("printf ' \\t\\n' > ${dir}/client-key")
        machine.succeed("systemctl restart cliproxy-api.service")
        machine.wait_until_succeeds("journalctl -u cliproxy-api | grep -q 'Config generation failed: No client API key resolved'")
        machine.fail(f"curl -s {API}/v1/models")
        machine.fail(f"test -e {CONFIG}")
  '';
}
