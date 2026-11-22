{ pkgs }:
let
  connectToken = "fake-connect-token";
  fakeConnect = pkgs.writers.writePython3 "fake-connect" { flakeIgnore = [ "E501" ]; } ''
    import json
    from http.server import BaseHTTPRequestHandler, HTTPServer

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

        def do_GET(self):
            if self.headers.get("Authorization") != "Bearer ${connectToken}":
                return self.answer(401, {"status": 401})
            parts = self.path.split("?")[0].strip("/").split("/")
            if parts == ["v1", "vaults"]:
                return self.answer(200, [{"id": VAULT, "name": "Agents"}])
            if parts == ["v1", "vaults", VAULT, "items"]:
                return self.answer(200, [{k: v for k, v in i.items() if k != "fields"} for i in ITEMS])
            if len(parts) == 5 and parts[:4] == ["v1", "vaults", VAULT, "items"]:
                for i in ITEMS:
                    if i["id"] == parts[4]:
                        return self.answer(200, i)
            return self.answer(404, {"status": 404})


    HTTPServer(("127.0.0.1", 8181), Handler).serve_forever()
  '';
in
pkgs.testers.runNixOSTest {
  name = "ts-broker";
  nodes.machine =
    { ... }:
    {
      imports = [ ../modules/services/ts-broker.nix ];
      virtualisation.memorySize = 2048;
      environment.systemPackages = [
        pkgs.curl
        pkgs.socat
        pkgs.sqlite
      ];
      modules.services.ts-broker = {
        enable = true;
        onepassword = {
          url = "http://127.0.0.1:8181";
          tokenFile = "/run/ts-broker-test/token";
        };
      };
      systemd.tmpfiles.rules = [
        "d /run/ts-broker-test 0700 root root -"
        "f+ /run/ts-broker-test/token 0600 root root - ${connectToken}"
      ];
      systemd.services.fake-connect = {
        wantedBy = [ "multi-user.target" ];
        before = [ "ts-broker.service" ];
        serviceConfig.ExecStart = fakeConnect;
      };
      systemd.services.ts-broker.after = [ "fake-connect.service" ];
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
    import shlex
    from datetime import timedelta

    SOCKET = "/run/ts-broker/decide.sock"
    UI = "http://127.0.0.1:795"
    ADMIN = "Tailscale-User-Login: admin@example"
    DATASTAR = ["Datastar-Request: true", "Sec-Fetch-Site: same-origin"]


    def decide(user="egress", **override):
        req = {"op": "decide", "identity": "tag:t3-alice", "node": "t3-alice",
               "groups": ["agents"], "modes": ["inject"], "credential": "GITHUB_TOKEN",
               "host": "api.github.com", "header": "Authorization", "mode": "inject"}
        req.update(override)
        line = shlex.quote(json.dumps(req))
        out = machine.succeed(f"printf '%s\\n' {line} | runuser -u {user} -- socat -t 5 - UNIX-CONNECT:{SOCKET}")
        return json.loads(out)


    def curl(path, headers, extra=()):
        args = ["curl", "-s", "-o", "/dev/null", "-w", "'%{http_code}'", *extra]
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
        return machine.succeed(f"curl -s -X POST {headers} --data {body} {shlex.quote(UI + path)}")


    def sql(query):
        return machine.succeed(f"sqlite3 -json /var/lib/ts-broker/broker.db {shlex.quote(query)}")


    machine.wait_for_unit("ts-broker.service")
    machine.wait_for_open_port(795, timeout=timedelta(seconds=60))
    machine.wait_until_succeeds(f"test -S {SOCKET}")

    with subtest("only the socket group can ask for decisions"):
        assert machine.succeed(f"stat -c '%a %G' {SOCKET}").strip() == "660 ts-broker-clients"
        machine.fail(f"echo '{{}}' | runuser -u outsider -- socat -t 2 - UNIX-CONNECT:{SOCKET}")
        assert decide(credential="GITHUB_TOKEN") == {"decision": "deny", "reason": "unknown"}

    with subtest("the UI listens on loopback and needs the Tailscale login"):
        listeners = machine.succeed("ss -Hltn 'sport = :795' | awk '{print $4}'").split()
        assert listeners == ["127.0.0.1:795"], listeners
        assert curl("/", []) == "403"
        assert act("/action/approval", {"id": 1, "event": "approve", "grant": "once"}, DATASTAR) == "403"
        assert curl("/", [ADMIN]) == "200"

    with subtest("only root, which tailscaled runs as, reaches the UI"):
        for user in ["outsider", "egress", "ts-broker"]:
            status, out = machine.execute(
                f"runuser -u {user} -- curl -s -o /dev/null -w '%{{http_code}}' --max-time 3 -H {shlex.quote(ADMIN)} {UI}/")
            assert status == 28 and out == "000", (user, status, out)
        assert "counter packets 0 " not in machine.succeed("nft list table inet ts-broker")

    with subtest("the broker holds no capability but binding its privileged port"):
        pid = machine.succeed("systemctl show -p MainPID --value ts-broker.service").strip()
        caps = dict(line.split(":\t") for line in machine.succeed(f"grep ^Cap /proc/{pid}/status").splitlines())
        assert {k: caps[k] for k in ["CapAmb", "CapBnd", "CapEff"]} == {
            "CapAmb": "0000000000000400", "CapBnd": "0000000000000400", "CapEff": "0000000000000400"}, caps

    with subtest("responses forbid framing and posts must be same-origin Datastar requests"):
        page_headers = machine.succeed(f"curl -s -D - -o /dev/null -H {shlex.quote(ADMIN)} {UI}/").lower()
        assert "content-security-policy: frame-ancestors 'none'" in page_headers, page_headers
        assert "x-frame-options: deny" in page_headers, page_headers
        refused = machine.succeed(f"curl -s -D - -o /dev/null {UI}/").lower()
        assert "x-frame-options: deny" in refused, refused
        delete = {"name": "GITHUB_TOKEN"}
        assert act("/action/delete-credential", delete, [ADMIN, "Datastar-Request: true"]) == "403"
        assert act("/action/delete-credential", delete, [ADMIN, "Datastar-Request: true", "Sec-Fetch-Site: cross-site"]) == "403"
        assert act("/action/delete-credential", delete, [ADMIN, "Sec-Fetch-Site: same-origin"]) == "403"

    with subtest("a credential is created from the catalog and assigned"):
        assert post("/action/create-credential", {
            "vault": "vlt0000000000000000000000a", "item": "itm0000000000000000000000a",
            "field": "fld0000000000000000000000a", "name": "GITHUB_TOKEN",
            "header": "Authorization", "hosts": "api.github.com"}) == "200"
        assert post("/action/assign", {"credential": "GITHUB_TOKEN", "subjects": "group:agents",
                                       "mode": "inject", "approval": "auto"}) == "200"
        cred = json.loads(sql("select field, field_label, editor from credential"))
        assert cred == [{"field": "fld0000000000000000000000a", "field_label": "credential",
                         "editor": "admin@example"}], cred

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
        assert post("/action/assign", {"credential": "GITHUB_TOKEN", "subjects": "group:agents",
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
        assert '"kind":"use"' in journal and "ghp_fake_secret" not in journal
        machine.fail("grep -r ghp_fake_secret /var/lib/ts-broker")

    with subtest("the UI never listens without its firewall rule, comes back with it, and no user can take its port"):
        machine.succeed("systemctl stop nftables.service")
        machine.wait_until_fails("systemctl is-active ts-broker.service", timeout=timedelta(seconds=30))
        machine.fail("nft list table inet ts-broker")
        assert machine.succeed("ss -Hltn 'sport = :795'").strip() == ""
        out = machine.fail("runuser -u outsider -- socat -T 1 TCP-LISTEN:795,bind=127.0.0.1 - 2>&1")
        assert "Permission denied" in out, out
        machine.succeed("systemctl start nftables.service")
        machine.wait_until_succeeds("systemctl is-active ts-broker.service", timeout=timedelta(seconds=30))
        machine.wait_for_open_port(795, timeout=timedelta(seconds=60))
        machine.succeed("nft list table inet ts-broker")
        assert curl("/", [ADMIN]) == "200"
  '';
}
