{ pkgs }:
let
  connectToken = "fake-connect-token";
  connectUrl = "http://op-connect.example.ts.net:8080";
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
        {"id": "itm0000000000000000000000a", "title": "GitHub token",
         "category": "API_CREDENTIAL", "vault": {"id": VAULT},
         "fields": [{"id": "fld0000000000000000000000a", "label": "credential",
                     "type": "CONCEALED", "value": "ghp_fake_secret"}]},
    ]


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
                return self.answer(200, [{"id": VAULT, "name": "Agents"}])
            if parts == ["v1", "vaults", VAULT, "items"]:
                return self.answer(200, [{k: v for k, v in i.items() if k != "fields"} for i in ITEMS])
            if len(parts) == 5 and parts[:4] == ["v1", "vaults", VAULT, "items"]:
                for i in ITEMS:
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
  name = "ts-broker";
  nodes.machine =
    { ... }:
    {
      imports = [
        ../modules/motd.nix
        ../modules/services/ts-broker.nix
      ];
      virtualisation.memorySize = 2048;
      environment.systemPackages = [
        pkgs.curl
        pkgs.nftables
        pkgs.socat
        pkgs.sqlite
      ];
      modules.services.ts-broker = {
        enable = true;
        authKeyFile = "/run/ts-broker-test/authkey";
        onepassword = {
          url = connectUrl;
          tokenFile = "/run/ts-broker-test/token";
        };
      };
      systemd.tmpfiles.rules = [
        "d /run/ts-broker-test 0700 root root -"
        "f+ /run/ts-broker-test/token 0600 root root - ${connectToken}"
        "f+ /run/ts-broker-test/authkey 0600 root root - tskey-auth-test"
      ];
      systemd.services.fake-connect = {
        requires = [ "ts-broker.service" ];
        after = [ "ts-broker.service" ];
        serviceConfig = {
          ExecStart = "${fakeConnect} /run/ts-broker/ui/proxy.sock";
          RuntimeDirectory = "fake-connect";
        };
      };
      users.users.egress = {
        isSystemUser = true;
        group = "egress";
        extraGroups = [ "ts-broker-clients" ];
      };
      users.groups.egress = { };
      users.users.outsider = {
        isSystemUser = true;
        group = "outsider";
      };
      users.groups.outsider = { };
    };

  testScript = ''
    import json
    import re
    import shlex
    from datetime import timedelta

    SOCKET = "/run/ts-broker/decide.sock"
    UI_DIR = "/run/ts-broker/ui"
    UI_SOCKET = f"{UI_DIR}/ui.sock"
    UI = "http://ts-broker.example.ts.net"
    ADMIN = "Tailscale-User-Login: admin@example"
    DATASTAR = ["Datastar-Request: true", "Sec-Fetch-Site: same-origin"]


    credential = "BRK_UNKNOWN0"


    def decide(user="egress", **override):
        req = {"op": "decide", "identity": "tag:t3-alice", "node": "t3-alice",
               "groups": ["agents"], "modes": ["inject"], "credential": credential,
               "host": "api.github.com", "header": "Authorization", "mode": "inject"}
        req.update(override)
        line = shlex.quote(json.dumps(req))
        out = machine.succeed(f"printf '%s\\n' {line} | runuser -u {user} -- socat -t 5 - UNIX-CONNECT:{SOCKET}")
        return json.loads(out)


    def ui(user="ts-broker"):
        """curl on the UI socket as user. ts-plug, which runs as ts-broker,
        sends every tailnet request this way with the caller's headers."""
        return f"runuser -u {user} -- curl -s --unix-socket {UI_SOCKET}"


    def curl(path, headers, extra=()):
        args = [ui(), "-o", "/dev/null", "-w", "'%{http_code}'", *extra]
        for h in headers:
            args += ["-H", shlex.quote(h)]
        return machine.succeed(" ".join(args + [shlex.quote(UI + path)])).strip()


    def post(path, form=None, headers=(ADMIN, *DATASTAR)):
        extra = ["-X", "POST"]
        for k, v in (form or {}).items():
            extra += ["--data-urlencode", shlex.quote(f"{k}={v}")]
        return curl(path, headers, extra)


    def act(path, payload, headers=(ADMIN, *DATASTAR)):
        body = shlex.quote(json.dumps(payload))
        return curl(path, [*headers, "Content-Type: application/json"], ["-X", "POST", "--data", body])


    def flash(path, payload):
        body = shlex.quote(json.dumps(payload))
        headers = " ".join("-H " + shlex.quote(h) for h in (ADMIN, *DATASTAR, "Content-Type: application/json"))
        return machine.succeed(f"{ui()} -X POST {headers} --data {body} {shlex.quote(UI + path)}")


    def sql(query):
        return machine.succeed(f"sqlite3 -json /var/lib/ts-broker/broker.db {shlex.quote(query)}")


    machine.wait_for_unit("ts-broker.service")
    machine.wait_until_succeeds(f"test -S {SOCKET} && test -S {UI_SOCKET}", timeout=timedelta(seconds=60))
    machine.succeed("systemctl start fake-connect.service")
    machine.wait_until_succeeds(f"test -S {UI_DIR}/proxy.sock")

    with subtest("only the socket group can ask for decisions"):
        assert machine.succeed(f"stat -c '%a %G' {SOCKET}").strip() == "660 ts-broker-clients"
        machine.fail(f"echo '{{}}' | runuser -u outsider -- socat -t 2 - UNIX-CONNECT:{SOCKET}")
        assert decide() == {"decision": "deny", "reason": "unknown"}

    with subtest("the UI is a socket that only ts-plug's user can reach"):
        assert machine.succeed(f"stat -c '%a %U' {UI_DIR} {UI_SOCKET}").split("\n")[:2] == ["700 ts-broker", "600 ts-broker"]
        for user in ["outsider", "egress"]:
            out = machine.fail(f"runuser -u {user} -- ls {UI_DIR} 2>&1")
            assert "Permission denied" in out, (user, out)
            status, out = machine.execute(f"{ui(user)} -o /dev/null -w '%{{http_code}}' -H {shlex.quote(ADMIN)} {UI}/")
            assert status == 7 and out == "000", (user, status, out)
        assert curl("/", [ADMIN]) == "200"

    with subtest("the UI needs a person's Tailscale login"):
        assert curl("/", []) == "403"
        assert curl("/", ["Tailscale-User-Login;", "Tailscale-Node-Tags: tag:t3-alice"]) == "403"
        assert act("/action/approval", {"id": 1, "event": "approve", "grant": "once"}, DATASTAR) == "403"

    with subtest("the broker opens no TCP port, has no firewall table and holds no capability"):
        cgroup = machine.succeed("systemctl show -p ControlGroup --value ts-broker.service").strip()
        pids = machine.succeed(f"cat /sys/fs/cgroup{cgroup}/cgroup.procs").split()
        assert len(pids) >= 2, pids
        listeners = machine.succeed("ss -Hltnp")
        assert not any(f"pid={pid}," in listeners for pid in pids), listeners
        assert "ts-broker" not in machine.succeed("nft list tables")
        for pid in pids:
            caps = dict(line.split(":\t") for line in machine.succeed(f"grep ^Cap /proc/{pid}/status").splitlines())
            assert {k: caps[k] for k in ["CapAmb", "CapBnd", "CapEff"]} == {
                "CapAmb": "0000000000000000", "CapBnd": "0000000000000000", "CapEff": "0000000000000000"}, (pid, caps)

    with subtest("responses forbid framing and posts must be same-origin Datastar requests"):
        page_headers = machine.succeed(f"{ui()} -D - -o /dev/null -H {shlex.quote(ADMIN)} {UI}/").lower()
        assert "content-security-policy: frame-ancestors 'none'" in page_headers, page_headers
        assert "x-frame-options: deny" in page_headers, page_headers
        refused = machine.succeed(f"{ui()} -D - -o /dev/null {UI}/").lower()
        assert "x-frame-options: deny" in refused, refused
        delete = {"name": credential}
        assert act("/action/delete-credential", delete, [ADMIN, "Datastar-Request: true"]) == "403"
        assert act("/action/delete-credential", delete, [ADMIN, "Datastar-Request: true", "Sec-Fetch-Site: cross-site"]) == "403"
        assert act("/action/delete-credential", delete, [ADMIN, "Sec-Fetch-Site: same-origin"]) == "403"

    with subtest("a credential is created from the catalog and assigned"):
        assert post("/action/create-credential", {
            "vault": "vlt0000000000000000000000a", "item": "itm0000000000000000000000a",
            "field": "fld0000000000000000000000a",
            "header": "Authorization", "hosts": "api.github.com"}) == "200"
        cred = json.loads(sql("select name, field, vault_title, item_title, field_label, editor from credential"))
        credential = cred[0]["name"]
        assert re.fullmatch("BRK_[A-Z2-7]{8}", credential), cred
        assert post("/action/assign", {"credential": credential, "subjects": "group:agents",
                                       "mode": "inject", "approval": "auto"}) == "200"
        assert cred == [{"name": credential, "field": "fld0000000000000000000000a", "vault_title": "Agents",
                         "item_title": "GitHub token", "field_label": "credential",
                         "editor": "admin@example"}], cred
        requests = machine.succeed("cat /run/fake-connect/requests").split()
        assert "http://op-connect.example.ts.net:8080/v1/vaults/vlt0000000000000000000000a/items/itm0000000000000000000000a" in requests, requests

    allow = {"decision": "allow", "approval": None,
             "secret": {"vault": "vlt0000000000000000000000a", "item": "itm0000000000000000000000a",
                        "field": "fld0000000000000000000000a"},
             "inject": {"header": "Authorization"}}

    with subtest("auto allows, and the ceiling, host and assignment deny"):
        assert decide() == allow
        assert decide(modes=[]) == {"decision": "deny", "reason": "mode"}
        assert decide(host="evil.example.com") == {"decision": "deny", "reason": "host"}
        assert decide(identity="tag:t3-bob", groups=[]) == {"decision": "deny", "reason": "unassigned"}

    with subtest("ask waits for an approval from the UI"):
        assert post("/action/assign", {"credential": credential, "subjects": "group:agents",
                                       "mode": "inject", "approval": "ask"}) == "200"
        assert decide() == {"decision": "pending", "approval": "1"}
        assert decide() == {"decision": "pending", "approval": "1"}
        assert "No such approval or event." in flash("/action/approval?id=1&event=approve&grant=forever", {})
        assert decide() == {"decision": "pending", "approval": "1"}, "the query string is ignored"
        assert act("/action/approval", {"id": 1, "event": "approve", "grant": "forever"}) == "200"
        assert decide() == dict(allow, approval="1")

    with subtest("revoke applies on the next request"):
        assert act("/action/approval", {"id": 1, "event": "revoke"}) == "200"
        assert decide() == {"decision": "pending", "approval": "2"}

    with subtest("deny and deny and block"):
        assert act("/action/approval", {"id": 2, "event": "deny"}) == "200"
        assert decide() == {"decision": "pending", "approval": "3"}
        assert act("/action/approval", {"id": 3, "event": "block"}) == "200"
        assert decide() == {"decision": "deny", "reason": "blocked"}

    with subtest("the audit log records requests, decisions, uses and changes"):
        rows = json.loads(sql("select kind, identity, actor, credential, detail from audit order by id"))
        kinds = {r["kind"] for r in rows}
        assert kinds == {"credential", "assignment", "request", "decision", "use", "approval"}, kinds
        approvals = [(json.loads(r["detail"])["state"], r["actor"]) for r in rows if r["kind"] == "approval"]
        assert approvals == [("pending", None), ("approved", "admin@example"), ("revoked", "admin@example"),
                             ("pending", None), ("denied", "admin@example"), ("pending", None),
                             ("blocked", "admin@example")], approvals
        uses = [r for r in rows if r["kind"] == "use"]
        assert [u["identity"] for u in uses] == ["tag:t3-alice", "tag:t3-alice"], uses
        journal = machine.succeed("journalctl -u ts-broker -o cat")
        assert "ghp_fake_secret" not in journal
        audit = [json.loads(line) for line in journal.splitlines() if line.startswith("{")]
        assert "use" in {row["kind"] for row in audit}, "ts-plug passes the audit rows through as JSON lines"
        machine.fail("grep -r ghp_fake_secret /var/lib/ts-broker")
  '';
}
