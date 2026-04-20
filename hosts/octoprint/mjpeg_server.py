#!/usr/bin/env python3
"""Minimal MJPEG HTTP server reading frames from rpicam-vid stdout.

Serves `?action=stream` and `?action=snapshot` for OctoPrint.
Only runs the camera process while clients are connected.
"""

import subprocess
import sys
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer
from socketserver import ThreadingMixIn


BOUNDARY = b"--FRAME"
lock = threading.Lock()
new_frame = threading.Condition(lock)
current_frame = b""
capture_cmd = None
capture_refcount = 0
capture_proc = None


def capture_loop(proc):
    global current_frame

    buf = bytearray()
    while True:
        chunk = proc.stdout.read(4096)
        if not chunk:
            break
        buf.extend(chunk)
        while True:
            start = buf.find(b"\xff\xd8")
            if start == -1:
                if buf.endswith(b"\xff"):
                    del buf[:-1]
                else:
                    buf.clear()
                break
            end = buf.find(b"\xff\xd9", start + 2)
            if end == -1:
                break
            frame = bytes(buf[start : end + 2])
            del buf[: end + 2]
            with new_frame:
                current_frame = frame
                new_frame.notify_all()

    with new_frame:
        new_frame.notify_all()


def acquire_capture():
    global capture_refcount
    global capture_proc
    global current_frame

    with new_frame:
        capture_refcount += 1
        if capture_proc is None:
            current_frame = b""
            capture_proc = subprocess.Popen(capture_cmd, stdout=subprocess.PIPE, bufsize=0)
            threading.Thread(target=capture_loop, args=(capture_proc,), daemon=True).start()


def release_capture():
    global capture_refcount
    global capture_proc

    proc_to_kill = None
    with new_frame:
        capture_refcount -= 1
        if capture_refcount <= 0:
            capture_refcount = 0
            proc_to_kill = capture_proc
            capture_proc = None
            new_frame.notify_all()

    if proc_to_kill:
        proc_to_kill.terminate()


class ThreadingHTTPServer(ThreadingMixIn, HTTPServer):
    daemon_threads = True


class MJPEGHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        try:
            if "action=snapshot" in self.path:
                acquire_capture()
                try:
                    with new_frame:
                        while not current_frame:
                            if not new_frame.wait(timeout=5):
                                self.send_error(503)
                                return
                        frame = current_frame
                finally:
                    release_capture()

                self.send_response(200)
                self.send_header("Content-Type", "image/jpeg")
                self.send_header("Content-Length", str(len(frame)))
                self.end_headers()
                self.wfile.write(frame)
            elif "action=stream" in self.path:
                self.send_response(200)
                self.send_header("Content-Type", "multipart/x-mixed-replace; boundary=FRAME")
                self.send_header("Cache-Control", "no-cache")
                self.end_headers()

                acquire_capture()
                try:
                    prev = None
                    while True:
                        with new_frame:
                            while current_frame is prev or not current_frame:
                                if not new_frame.wait(timeout=5):
                                    return
                            frame = current_frame
                        self.wfile.write(BOUNDARY + b"\r\n")
                        self.wfile.write(b"Content-Type: image/jpeg\r\n")
                        self.wfile.write(f"Content-Length: {len(frame)}\r\n\r\n".encode("ascii"))
                        self.wfile.write(frame)
                        self.wfile.flush()
                        prev = frame
                finally:
                    release_capture()
            else:
                self.send_error(404)
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass

    def log_message(self, fmt, *args):
        del fmt, args


if __name__ == "__main__":
    port = int(sys.argv[1])
    capture_cmd = sys.argv[2:]
    ThreadingHTTPServer(("0.0.0.0", port), MJPEGHandler).serve_forever()
