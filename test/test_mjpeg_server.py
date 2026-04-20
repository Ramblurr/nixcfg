import http.client
import socket
import subprocess
import sys
import tempfile
import textwrap
import time
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
SERVER_SCRIPT = REPO_ROOT / "hosts" / "octoprint" / "mjpeg_server.py"


def find_free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


class MJPEGServerTest(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.marker_path = self.tmp_path / "producer-started"
        self.producer_script = self.tmp_path / "producer.py"
        self.producer_script.write_text(
            textwrap.dedent(
                """
                import signal
                import sys
                import time
                from pathlib import Path

                marker = Path(sys.argv[1])
                marker.write_text("started")
                frame = b"\\xff\\xd8test-frame\\xff\\xd9"

                def stop(*_args):
                    raise SystemExit(0)

                signal.signal(signal.SIGTERM, stop)

                while True:
                    sys.stdout.buffer.write(frame)
                    sys.stdout.buffer.flush()
                    time.sleep(0.05)
                """
            ),
            encoding="ascii",
        )
        self.port = find_free_port()
        self.server_proc = subprocess.Popen(
            [
                sys.executable,
                str(SERVER_SCRIPT),
                str(self.port),
                sys.executable,
                str(self.producer_script),
                str(self.marker_path),
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.wait_for_server()

    def tearDown(self):
        if self.server_proc.poll() is None:
            self.server_proc.terminate()
            try:
                self.server_proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.server_proc.kill()
                self.server_proc.wait(timeout=5)
        if self.server_proc.stdout is not None:
            self.server_proc.stdout.close()
        if self.server_proc.stderr is not None:
            self.server_proc.stderr.close()
        self.tmpdir.cleanup()

    def wait_for_server(self):
        deadline = time.time() + 5
        while time.time() < deadline:
            if self.server_proc.poll() is not None:
                stdout, stderr = self.server_proc.communicate(timeout=1)
                self.fail(
                    f"server exited before accepting connections: {stdout!r} {stderr!r}"
                )
            try:
                with socket.create_connection(("127.0.0.1", self.port), timeout=0.2):
                    return
            except OSError:
                time.sleep(0.05)
        self.fail("server did not start listening in time")

    def test_snapshot_returns_jpeg_and_starts_capture_lazily(self):
        self.assertFalse(self.marker_path.exists())

        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=5)
        conn.request("GET", "/?action=snapshot")
        resp = conn.getresponse()
        body = resp.read()
        conn.close()

        self.assertEqual(resp.status, 200)
        self.assertEqual(resp.getheader("Content-Type"), "image/jpeg")
        self.assertTrue(body.startswith(b"\xff\xd8"))
        self.assertTrue(body.endswith(b"\xff\xd9"))
        self.assertTrue(self.marker_path.exists())

    def test_stream_returns_multipart_mjpeg(self):
        with socket.create_connection(("127.0.0.1", self.port), timeout=5) as sock:
            sock.settimeout(5)
            sock.sendall(
                b"GET /?action=stream HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
            )
            data = b""
            while b"\xff\xd8test-frame\xff\xd9" not in data:
                data += sock.recv(4096)

        self.assertIn(b"200 OK", data)
        self.assertIn(b"multipart/x-mixed-replace; boundary=FRAME", data)
        self.assertIn(b"--FRAME", data)
        self.assertIn(b"\xff\xd8test-frame\xff\xd9", data)

    def test_unknown_path_returns_404(self):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=5)
        conn.request("GET", "/")
        resp = conn.getresponse()
        resp.read()
        conn.close()

        self.assertEqual(resp.status, 404)


if __name__ == "__main__":
    unittest.main()
