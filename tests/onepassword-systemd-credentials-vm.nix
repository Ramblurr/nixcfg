{ inputs, pkgs }:
let
  tls = import ../configs/home-ops/onepassword-connect-tls.nix;
  token = "runtime-bootstrap-token";
  virtualAddress = "192.0.2.22";
  port = toString tls.port;
  outageClock = 1577836800;
  certificates =
    pkgs.runCommand "onepassword-provider-test-certificates"
      {
        nativeBuildInputs = [ pkgs.openssl ];
      }
      ''
        for address in 127.0.0.1 10.9.8.22 10.9.4.17; do
          openssl verify -attime ${toString outageClock} -partial_chain -purpose sslserver \
            -verify_ip $address -trusted ${tls.certificate} ${tls.certificate}
        done
        mkdir $out
        for name in connect squatter; do
          openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:P-256 -nodes \
            -not_before 20000101000000Z -not_after 20460101000000Z \
            -subj "/CN=$name" \
            -addext "subjectAltName=IP:127.0.0.1,IP:${virtualAddress}" \
            -addext "basicConstraints=critical,CA:FALSE" \
            -addext "extendedKeyUsage=serverAuth" \
            -keyout $out/$name.key -out $out/$name.crt
        done
      '';
  fakeConnect = pkgs.writers.writePython3 "fake-connect" { flakeIgnore = [ "E501" ]; } ''
    import json
    import re
    import ssl
    import sys
    import threading
    import time
    from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

    address, identity = sys.argv[1], sys.argv[2]
    VAULT = {"id": "vlt0000000000000000000000a", "name": "test"}
    attempts = {}
    lock = threading.Lock()


    def value(title):
        if title == "Example":
            return f"{identity}-credential-value"
        if title == "local-down":
            return None if identity == "local" else f"{identity}-credential-value"
        if title == "transient":
            with lock:
                attempts[title] = attempts.get(title, 0) + 1
                return "recovered-credential-value" if attempts[title] >= 3 else None
        if title == "hang":
            time.sleep(60)
        if title == "empty":
            return ""
        if title == "oversized":
            return "x" * (1024 * 1024 + 1)
        return None


    def item(title, secret):
        return {
            "id": "itm0000000000000000000000a",
            "title": title,
            "category": "PASSWORD",
            "vault": {"id": VAULT["id"]},
            "fields": [{"id": "password", "label": "password", "type": "CONCEALED", "purpose": "PASSWORD", "value": secret}],
        }


    class Handler(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"
        secret = None

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
            if self.headers.get("Authorization") != "Bearer ${token}":
                return self.answer(401, {"status": 401, "message": "unauthorized"})
            path, _, query = self.path.partition("?")
            items = f"/v1/vaults/{VAULT['id']}/items"
            if path == "/v1/vaults":
                return self.answer(200, [VAULT])
            if path == items:
                match = re.search(r'title\+eq\+%22([^%]+)%22', query)
                title = match.group(1) if match else ""
                Handler.secret = value(title)
                if Handler.secret is None:
                    return self.answer(200, [])
                return self.answer(200, [{k: v for k, v in item(title, "").items() if k != "fields"}])
            if path.startswith(items + "/") and Handler.secret is not None:
                return self.answer(200, item("", Handler.secret))
            return self.answer(404, {"status": 404, "message": "not found"})


    server = ThreadingHTTPServer((address, ${port}), Handler)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain("${certificates}/connect.crt", "${certificates}/connect.key")
    server.socket = context.wrap_socket(server.socket, server_side=True)
    server.serve_forever()
  '';
  squatter = pkgs.writers.writePython3 "port-squatter" { flakeIgnore = [ "E501" ]; } ''
    import json
    import socket
    import socketserver
    import ssl
    import sys

    address, port, mode = sys.argv[1], int(sys.argv[2]), sys.argv[3]
    context = None
    if mode == "tls":
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain("${certificates}/squatter.crt", "${certificates}/squatter.key")


    class Handler(socketserver.BaseRequestHandler):
        def handle(self):
            connection = self.request
            connection.settimeout(3)
            record = {"local": connection.getsockname()[0], "mode": mode}
            try:
                if context is not None:
                    connection = context.wrap_socket(connection, server_side=True)
                    record["handshake"] = "ok"
                received = b""
                while b"\r\n\r\n" not in received and len(received) < 65536:
                    chunk = connection.recv(4096)
                    if not chunk:
                        break
                    received += chunk
                record["bytes"] = len(received)
                record["headers"] = [line for line in received.decode("latin-1").split("\r\n") if ":" in line and line.isprintable()]
                connection.sendall(b"HTTP/1.1 503 Service Unavailable\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
            except (OSError, ssl.SSLError) as error:
                record["error"] = type(error).__name__
            print(json.dumps(record), flush=True)


    class Server(socketserver.ThreadingTCPServer):
        allow_reuse_address = True
        address_family = socket.AF_INET


    Server((address, port), Handler).serve_forever()
  '';
  connectService = address: identity: {
    serviceConfig = {
      User = "connect";
      ExecStart = "${fakeConnect} ${address} ${identity}";
    };
  };
  squatterService = address: squatterPort: mode: {
    serviceConfig = {
      User = "squatter";
      ExecStart = "${squatter} ${address} ${toString squatterPort} ${mode}";
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "onepassword-systemd-credentials";

  nodes.machine = {
    imports = [
      inputs.sops-nix.nixosModules.sops
      ../modules/services/onepassword-systemd-credentials.nix
    ];

    options.site = pkgs.lib.mkOption {
      type = pkgs.lib.types.attrs;
      default = { };
    };

    config = {
      networking.hostName = "debord";
      site.net.mgmt.hosts4.onepassword-connect = [ virtualAddress ];
      system.stateVersion = "26.05";

      security.pki.certificateFiles = [
        "${certificates}/connect.crt"
        "${certificates}/squatter.crt"
      ];

      modules.services.onepassword-systemd-credentials = {
        connectHosts = [
          (tls.url "127.0.0.1")
          (tls.url virtualAddress)
        ];
        connectCertificate = "${certificates}/connect.crt";
        bootstrapTokenFile = "/run/onepassword-provider-test-token";
        consumers = {
          credential-success.password = "op://test/Example/password";
          credential-old-clock.password = "op://test/Example/password";
          credential-fallback.password = "op://test/local-down/password";
          credential-squatted-local.password = "op://test/Example/password";
          credential-squatted.password = "op://test/Example/password";
          credential-hang.password = "op://test/hang/password";
          credential-transient.password = "op://test/transient/password";
          credential-empty.password = "op://test/empty/password";
          credential-oversized.password = "op://test/oversized/password";
          credential-failure.password = "op://test/failure/password";
        };
      };

      users.groups.listeners = { };
      users.users.connect = {
        isSystemUser = true;
        group = "listeners";
      };
      users.users.squatter = {
        isSystemUser = true;
        group = "listeners";
      };

      systemd.services.connect-local = connectService "127.0.0.1" "local";
      systemd.services.connect-fallback = connectService virtualAddress "fallback";
      systemd.services.squatter-local = squatterService "127.0.0.1" tls.port "tls";
      systemd.services.squatter-tls = squatterService "0.0.0.0" tls.port "tls";
      systemd.services.squatter-plain = squatterService "0.0.0.0" tls.port "plain";
      systemd.services.squatter-old-port = squatterService "0.0.0.0" 8080 "plain";

      systemd.services.prepare-provider-token = {
        serviceConfig.Type = "oneshot";
        script = ''
          umask 077
          printf %s ${token} > /run/onepassword-provider-test-token
        '';
      };

      systemd.services.credential-success = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = local-credential-value
          touch /run/credential-success
        '';
      };

      systemd.services.credential-old-clock = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = local-credential-value
          touch /run/credential-old-clock
        '';
      };

      systemd.services.credential-fallback = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = fallback-credential-value
          touch /run/credential-fallback
        '';
      };

      systemd.services.credential-squatted-local = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = fallback-credential-value
          touch /run/credential-squatted-local
        '';
      };

      systemd.services.credential-squatted = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-squatted-unexpected";
      };

      systemd.services.credential-hang = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-hang-unexpected";
      };

      systemd.services.credential-transient = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = recovered-credential-value
          touch /run/credential-transient
        '';
      };

      systemd.services.credential-empty = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-empty-unexpected";
      };

      systemd.services.credential-oversized = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-oversized-unexpected";
      };

      systemd.services.credential-failure = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-failure-unexpected";
      };

      systemd.services.credential-unauthorized = {
        requires = [ "onepassword-credential-provider.socket" ];
        after = [ "onepassword-credential-provider.socket" ];
        serviceConfig = {
          Type = "oneshot";
          LoadCredential = [
            "password:/run/onepassword-credential-provider.sock"
          ];
          ExecStartPre = [ "${pkgs.coreutils}/bin/test -s %d/password" ];
        };
        script = "touch /run/credential-unauthorized-unexpected";
      };
    };
  };

  testScript = ''
    import json
    import time


    def squatter_records(unit):
        lines = machine.succeed(f"journalctl --no-pager -o cat -u {unit}").splitlines()
        return [json.loads(line) for line in lines if line.startswith("{")]


    def assert_token_withheld(unit, addresses):
        records = squatter_records(unit)
        print(unit, records)
        seen = {record["local"] for record in records}
        assert addresses <= seen, f"{unit} saw {seen}, expected {addresses}"
        assert not any("${token}" in header for record in records for header in record.get("headers", [])), f"{unit} received the token"


    def swap(stop, start):
        if stop:
            machine.succeed(f"systemctl stop {' '.join(stop)}")
            machine.wait_until_fails("ss -Hltn 'sport = :${port}' | grep -q .")
        machine.succeed(f"systemctl start {' '.join(start)}")


    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("ip addr add ${virtualAddress}/32 dev eth1")
    machine.succeed("systemctl start prepare-provider-token.service squatter-old-port.service")
    machine.wait_for_unit("onepassword-credential-provider.socket")
    machine.wait_until_succeeds("ss -Hltn 'sport = :8080' | grep -q .")

    swap([], ["connect-local.service", "connect-fallback.service"])
    machine.wait_for_open_port(${port}, "127.0.0.1")
    machine.wait_for_open_port(${port}, "${virtualAddress}")

    machine.succeed("systemctl start credential-success.service")
    machine.succeed("test -e /run/credential-success")

    now = machine.succeed("date +%s").strip()
    machine.succeed("date -u -s @${toString outageClock}")
    assert machine.succeed("date -u +%Y").strip() == "2020"
    machine.succeed("systemctl start credential-old-clock.service")
    machine.succeed("test -e /run/credential-old-clock")
    machine.succeed(f"date -u -s @{now}")

    machine.succeed("systemctl start credential-fallback.service")
    machine.succeed("test -e /run/credential-fallback")

    machine.succeed("systemctl start credential-transient.service")
    machine.succeed("test -e /run/credential-transient")

    start = time.monotonic()
    machine.fail("systemctl start credential-hang.service")
    elapsed = time.monotonic() - start
    assert 14.5 <= elapsed < 15.7, f"hanging lookup took {elapsed:.2f}s"
    machine.fail("test -e /run/credential-hang-unexpected")

    for unit in [
        "credential-empty.service",
        "credential-oversized.service",
        "credential-failure.service",
        "credential-unauthorized.service",
    ]:
        machine.fail(f"systemctl start {unit}")

    machine.fail("test -e /run/credential-empty-unexpected")
    machine.fail("test -e /run/credential-oversized-unexpected")
    machine.fail("test -e /run/credential-failure-unexpected")
    machine.fail("test -e /run/credential-unauthorized-unexpected")

    machine.succeed("systemctl stop connect-local.service")
    machine.wait_until_fails("ss -Hltn 'sport = :${port}' | grep -qF '127.0.0.1:${port}'")
    machine.succeed("systemctl start squatter-local.service")
    machine.wait_for_open_port(${port}, "127.0.0.1")
    machine.succeed("systemctl start credential-squatted-local.service")
    machine.succeed("test -e /run/credential-squatted-local")
    assert_token_withheld("squatter-local.service", {"127.0.0.1"})

    for mode, stop in [("tls", ["connect-fallback.service", "squatter-local.service"]), ("plain", ["squatter-tls.service"])]:
        swap(stop, [f"squatter-{mode}.service"])
        machine.wait_for_open_port(${port}, "${virtualAddress}")
        machine.fail("systemctl start credential-squatted.service")
        machine.fail("test -e /run/credential-squatted-unexpected")
        assert_token_withheld(f"squatter-{mode}.service", {"127.0.0.1", "${virtualAddress}"})

    assert squatter_records("squatter-old-port.service") == [], "a client used the old Connect port"

    journal = machine.succeed(
        "journalctl --no-pager -u 'onepassword-credential-provider@*.service'"
    )
    assert "${token}" not in journal
    assert "local-credential-value" not in journal
    assert "fallback-credential-value" not in journal
    assert "recovered-credential-value" not in journal
  '';
}
