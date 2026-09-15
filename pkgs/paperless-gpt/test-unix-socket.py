"""Exercise the packaged UI/API over UDS with a local, empty Paperless fixture."""
import http.client
import http.server
import json
import os
from pathlib import Path
import socket
import stat
import subprocess
import sys
import tempfile
import threading
import time


class EmptyPaperless(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_json({"count": 0, "next": None, "previous": None, "results": []})

    def do_POST(self):
        data = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))))
        self.send_json({"id": 1, **data})

    def send_json(self, data):
        body = json.dumps(data).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *args):
        pass


class UnixHTTPConnection(http.client.HTTPConnection):
    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.settimeout(5)
        self.sock.connect(self.host)


package = Path(sys.argv[1])
with tempfile.TemporaryDirectory() as tmp:
    work = Path(tmp)
    uds = str(work / "http.sock")
    (work / "default_prompts").symlink_to(package / "share/paperless-gpt/default_prompts")
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), EmptyPaperless)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    env = dict(os.environ, PAPERLESS_BASE_URL=f"http://127.0.0.1:{server.server_port}",
               PAPERLESS_API_TOKEN="synthetic-paperless-token", MISTRAL_API_KEY="synthetic-mistral-key",
               LLM_PROVIDER="mistral", LLM_MODEL="mistral-small-latest",
               OCR_PROVIDER="mistral_ocr", MISTRAL_MODEL="mistral-ocr-latest",
               OCR_PROCESS_MODE="whole_pdf", LISTEN_SOCKET=uds,
               LISTEN_INTERFACE="invalid-tcp-address", LOG_LEVEL="info")
    try:
        for attempt in range(2):
            # Mirrors systemd preStart, including cleanup after an unclean stop.
            Path(uds).unlink(missing_ok=True)
            with (work / "process.log").open("w+") as log:
                proc = subprocess.Popen([str(package / "bin/paperless-gpt")], cwd=work,
                                        env=env, stdout=log, stderr=log, umask=0o007)
                try:
                    for _ in range(300):
                        if Path(uds).exists():
                            break
                        if proc.poll() is not None:
                            raise AssertionError("sidecar exited before creating its socket")
                        time.sleep(0.1)
                    assert Path(uds).exists(), "socket startup timed out"
                    assert stat.S_IMODE(Path(uds).stat().st_mode) == 0o770
                    for path in ["/", "/api/config"]:
                        connection = UnixHTTPConnection(uds)
                        connection.request("GET", path, headers={"Host": "localhost"})
                        response = connection.getresponse()
                        body = response.read()
                        assert response.status == 200, (path, response.status, body)
                        assert body, path
                        if path == "/":
                            assert b"<html" in body.lower(), "frontend missing"
                        connection.close()
                    prompts = work / "prompts"
                    assert prompts.is_dir(), "editable prompts were not initialized"
                    marker = prompts / "restart-marker"
                    if attempt == 0:
                        marker.write_text("keep custom prompts")
                    else:
                        assert marker.read_text() == "keep custom prompts"
                except BaseException:
                    log.seek(0)
                    print(log.read())
                    raise
                finally:
                    proc.terminate()
                    try:
                        proc.wait(timeout=10)
                    except subprocess.TimeoutExpired:
                        proc.kill()
                        proc.wait()
        print("Packaged UDS UI/API, socket mode, and restart persistence passed.")
    finally:
        server.shutdown()
        server.server_close()
