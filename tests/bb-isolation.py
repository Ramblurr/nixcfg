"""Exercise the actual proxy and SMOLMEG2 framing, including bypass attempts."""
import asyncio
import functools
import http.client
import http.server
import ipaddress
import os
from pathlib import Path
import socket
import subprocess
import tempfile
import threading
import time
import unittest

import egress


class Policy(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.token = bytes(range(32))
        self.gateway = ipaddress.ip_address("100.64.1.2")
        self.denied = [ipaddress.ip_network("93.184.216.0/24")]
        self.server = await asyncio.start_server(functools.partial(
            egress.decide, token=self.token, gateway=self.gateway,
            gateway_port=8445, denied=self.denied), "127.0.0.1", 0)
        self.port = self.server.sockets[0].getsockname()[1]

    async def asyncTearDown(self):
        self.server.close()
        await self.server.wait_closed()

    async def request(self, address, port, token=None, length=0):
        address = ipaddress.ip_address(address)
        r, w = await asyncio.open_connection("127.0.0.1", self.port)
        w.write(b"SMOLMEG2" + (self.token if token is None else token)
                + bytes(32) + bytes([address.version]) + port.to_bytes(2, "big")
                + address.packed + length.to_bytes(2, "big"))
        await w.drain()
        result = await asyncio.wait_for(r.read(1), 2)
        w.close()
        await w.wait_closed()
        return result

    async def test_destinations(self):
        for address, port, expected in [
            ("100.64.1.2", 8445, True), ("100.64.1.2", 8444, False),
            ("100.64.1.2", 38886, False), ("100.64.1.2", 443, False),
            ("127.0.0.1", 443, False), ("10.0.0.1", 443, False),
            ("169.254.169.254", 80, False), ("100.96.0.1", 80, False),
            ("100.64.1.3", 8445, False), ("224.0.0.1", 443, False),
            ("::1", 443, False), ("fd7a:115c:a1e0::1", 443, False),
            ("::ffff:100.64.1.2", 8444, False),
            ("::ffff:100.64.1.2", 8445, True),
            ("2002:7f00:1::", 443, False),
            ("2606:4700:4700::1111", 443, True),
            ("1.1.1.1", 443, True), ("1.1.1.1", 80, True),
            ("1.1.1.1", 22, False), ("93.184.216.34", 443, False),
        ]:
            with self.subTest(address=address, port=port):
                self.assertEqual(await self.request(address, port), b"\0" if expected else b"\1")

    async def test_invalid_token_and_oversize(self):
        self.assertEqual(await self.request("1.1.1.1", 443, token=bytes(32)), b"")
        self.assertEqual(await self.request("1.1.1.1", 443, length=8193), b"")


class Redirect(unittest.IsolatedAsyncioTestCase):
    """Placeholder services: the broker carries bytes to the host target."""

    async def asyncSetUp(self):
        self.token = bytes(range(32))
        self.received = []

        async def origin(r, w):
            data = await r.readexactly(10)
            self.received.append(data)
            w.write(b"origin:" + data)
            await w.drain()
            w.close()

        self.origin = await asyncio.start_server(origin, "127.0.0.1", 0)
        origin_port = self.origin.sockets[0].getsockname()[1]
        dead = socket.socket()
        dead.bind(("127.0.0.1", 0))
        dead_port = dead.getsockname()[1]
        dead.close()
        services = dict(egress.parse_service(value) for value in [
            f"192.0.2.1:8445=127.0.0.1:{origin_port}",
            f"192.0.2.3:8445=127.0.0.1:{dead_port}"])
        self.server = await asyncio.start_server(functools.partial(
            egress.decide, token=self.token, gateway=ipaddress.ip_address("100.64.1.2"),
            gateway_port=8445, denied=[], services=services), "127.0.0.1", 0)
        self.port = self.server.sockets[0].getsockname()[1]

    async def asyncTearDown(self):
        for server in (self.server, self.origin):
            server.close()
            await server.wait_closed()

    async def flow(self, address, port, initial=b"", rest=b""):
        address = ipaddress.ip_address(address)
        r, w = await asyncio.open_connection("127.0.0.1", self.port)
        w.write(b"SMOLMEG2" + self.token + bytes(32) + bytes([address.version])
                + port.to_bytes(2, "big") + address.packed
                + len(initial).to_bytes(2, "big") + initial)
        await w.drain()
        decision = await asyncio.wait_for(r.read(1), 2)
        body = b""
        if decision == b"\x02":
            w.write(rest)
            await w.drain()
            body = await asyncio.wait_for(r.read(), 2)
        w.close()
        return decision, body

    async def test_known_placeholder_is_carried_with_initial_bytes_first(self):
        self.assertEqual(await self.flow("192.0.2.1", 8445, b"hello", b"world"),
                         (b"\x02", b"origin:helloworld"))
        self.assertEqual(await self.flow("::ffff:192.0.2.1", 8445, b"mapped", b"flow"),
                         (b"\x02", b"origin:mappedflow"))
        self.assertEqual(self.received, [b"helloworld", b"mappedflow"])

    async def test_other_placeholders_and_dead_targets_are_denied(self):
        for address, port in [("192.0.2.1", 8444), ("192.0.2.2", 8445),
                              ("192.0.2.1", 443), ("192.0.2.3", 8445)]:
            with self.subTest(address=address, port=port):
                self.assertEqual(await self.flow(address, port), (b"\x01", b""))
        self.assertEqual(await self.flow("1.1.1.1", 443), (b"\x00", b""))

    def test_placeholders_must_be_test_net_1(self):
        for value in ["100.64.1.2:8445=127.0.0.1:1", "192.0.3.1:1=127.0.0.1:1", "192.0.2.1:1"]:
            with self.subTest(value=value):
                with self.assertRaises(ValueError):
                    egress.parse_service(value)


class Backend(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.end_headers()
        self.wfile.write(self.path.encode())

    do_POST = do_GET

    def log_message(self, *args):
        pass


class Gateway(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        cls.backend = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Backend)
        threading.Thread(target=cls.backend.serve_forever, daemon=True).start()
        with socket.socket() as sock:
            sock.bind(("127.0.0.1", 0))
            cls.port = sock.getsockname()[1]
        template = Path(os.environ["GATEWAY_TEMPLATE"]).read_text()
        config = Path(cls.tmp.name) / "Caddyfile"
        config.write_text(template.replace("@listenPort@", str(cls.port)).replace(
            "@backendPort@", str(cls.backend.server_port)))
        cls.log = open(Path(cls.tmp.name) / "log", "w+")
        cls.process = subprocess.Popen([os.environ["CADDY"], "run", "--config", str(config),
                                        "--adapter", "caddyfile"], stdout=cls.log, stderr=cls.log,
                                       env=dict(os.environ, XDG_DATA_HOME=cls.tmp.name,
                                                XDG_CONFIG_HOME=cls.tmp.name))
        for _ in range(100):
            try:
                with socket.create_connection(("127.0.0.1", cls.port), timeout=.1):
                    return
            except OSError:
                if cls.process.poll() is not None:
                    cls.log.seek(0)
                    raise RuntimeError(cls.log.read())
                time.sleep(.05)
        raise RuntimeError("Caddy did not start")

    @classmethod
    def tearDownClass(cls):
        cls.process.terminate()
        cls.process.wait(timeout=5)
        cls.backend.shutdown()
        cls.backend.server_close()
        cls.log.close()
        cls.tmp.cleanup()

    def request(self, method, path):
        connection = http.client.HTTPConnection("127.0.0.1", self.port, timeout=3)
        connection.request(method, path, headers={"Host": "external.example.test"})
        response = connection.getresponse()
        result = response.status, response.read().decode()
        connection.close()
        return result

    def test_routes(self):
        digest = "a" * 64
        for method, path in [
            ("GET", "/internal/ws?sessionId=example"),
            ("GET", "/install.sh"), ("GET", "/install/version"),
            ("GET", "/install/bb-app.tgz"),
            ("GET", f"/internal/plugins/pi/host/{digest}"),
            ("GET", f"/internal/skills/tree/{digest}"),
            ("POST", f"/internal/skills/tree/{digest}/content"),
            *[("POST", path) for path in ["/internal/hosts/enroll", "/internal/session/open",
                "/internal/session/events", "/internal/session/interactive-request",
                "/internal/session/interactive-request/interrupt"]],
        ]:
            with self.subTest(method=method, path=path):
                self.assertEqual(self.request(method, path), (200, path))

    def test_denied_routes_and_bypasses(self):
        for path in ["/", "/api/v1/hosts", "/api/v1/threads", "/ws", "/ws/terminals/x",
                     "/internal/hosts/enroll-key", "/internal/session/tool-call",
                     "/internal/server-move/pending", "/internal/new-upstream-route",
                     "//api/v1/hosts", "/internal/../api/v1/hosts",
                     "/internal/%2e%2e/api/v1/hosts", "/internal/ws/../../api/v1/hosts",
                     "/internal/ws%2f..%2f..%2fapi/v1/hosts",
                     "/internal/ws/..%5c..%5capi/v1/hosts", "/INTERNAL/WS"]:
            for method in ["GET", "POST", "PUT", "DELETE", "OPTIONS", "CONNECT"]:
                with self.subTest(method=method, path=path):
                    self.assertEqual(self.request(method, path)[0], 403)
        for method, path in [("GET", "/internal/session/open"),
                             ("POST", "/internal/ws"), ("DELETE", "/install.sh")]:
            self.assertEqual(self.request(method, path)[0], 403)


if __name__ == "__main__":
    unittest.main()
