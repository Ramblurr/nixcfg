#!/usr/bin/env python3

from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import subprocess
import os
import sys
import argparse


parser = argparse.ArgumentParser()
parser.add_argument(
    "--dry-run", action="store_true", help="Enable dry run to not actually shut down"
)
parser.add_argument(
    "--timeout", type=int, default=60000, help="Timeout before shutdown in milliseconds"
)
parser.add_argument(
    "--port", type=int, default=5001, help="Port to listen on for shutdown requests"
)
args = parser.parse_args()

DRY_RUN = args.dry_run
TIMEOUT = str(args.timeout)
PORT = args.port
SHUTDOWN_BEARER_TOKEN = os.environ.get("HA_SHUTDOWN_TOKEN")
if SHUTDOWN_BEARER_TOKEN is None:
    print("Cannot load HA_SHUTDOWN_TOKEN")
    sys.exit(1)


class WebhookHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            if self.path == "/shutdown":
                auth_header = self.headers.get("Authorization", "")
                token = auth_header.split(" ")[-1]

                if token != SHUTDOWN_BEARER_TOKEN:
                    self.send_response(401)
                    self.end_headers()
                    self.wfile.write(json.dumps({"error": "Unauthorized"}).encode())
                    return

                result = subprocess.run(
                    [
                        "dunstify",
                        "--urgency",
                        "critical",
                        "--timeout",
                        TIMEOUT,
                        "--appname",
                        "update",
                        "--action",
                        "Y,yes",
                        "Shutting Down!",
                        "Click To Stop Shutdown",
                    ],
                    capture_output=True,
                    text=True,
                )

                output = result.stdout.strip()
                self.send_response(200)
                self.end_headers()

                if output == "Y":
                    self.wfile.write(
                        json.dumps({"message": "Shutdown aborted"}).encode()
                    )
                    print("ABORTING SHUTDOWN")
                    return

                if output == "1":
                    if DRY_RUN:
                        print("Would have powered off")
                    else:
                        print("POWERING OFF")
                        subprocess.run(["systemctl", "poweroff"])
                    self.wfile.write(json.dumps({"message": "Shutting down"}).encode())
                    return

                print("Unknown output from dunstify: " + output)
                self.wfile.write(
                    json.dumps({"message": "Unknown output from dunstify"}).encode()
                )
                return
        except Exception as e:
            print("ERROR: " + str(e))


print("Starting shutdown server on port " + str(PORT))
server_address = ("0.0.0.0", PORT)
httpd = HTTPServer(server_address, WebhookHandler)
httpd.serve_forever()
