#!/usr/bin/env python3
"""Harmless dunstify/mako protocol probe on a private D-Bus session.

Requires an existing Wayland display. Displays only labelled test notifications;
never runs the bridge or systemctl. Does not touch the session's normal mako.
"""

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time


def probe():
    with tempfile.TemporaryDirectory(prefix="ha-mqtt-notification-") as tmp:
        config = Path(tmp) / "config"
        config.write_text("font=monospace 18\nanchor=top-center\nwidth=600\n")
        daemon = subprocess.Popen(
            ["mako", "--config", str(config)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        try:
            time.sleep(0.5)
            assert daemon.poll() is None
            caps = subprocess.check_output(["dunstify", "--capabilities"], text=True)
            assert "actions" in caps.splitlines()
            for outcome in ["cancel", "dismiss", "expiry", "daemon-loss"]:
                start = time.monotonic()
                notification = subprocess.Popen(
                    [
                        "dunstify",
                        "--block",
                        "--urgency",
                        "critical",
                        "--appname",
                        "ha-mqtt",
                        "--timeout",
                        "1500",
                        "--action",
                        "cancel,Cancel shutdown",
                        "HA MQTT harmless test",
                        f"Testing {outcome}. No shutdown will occur.",
                    ],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                )
                try:
                    time.sleep(0.3)
                    if outcome == "cancel":
                        subprocess.run(["makoctl", "invoke", "cancel"], check=True)
                    elif outcome == "dismiss":
                        subprocess.run(["makoctl", "dismiss"], check=True)
                    elif outcome == "daemon-loss":
                        daemon.terminate()
                        daemon.wait(timeout=3)
                    try:
                        output, error = notification.communicate(timeout=3)
                        if outcome == "expiry":
                            assert (
                                notification.returncode == 0 and output.strip() == "1"
                            ), (output, error)
                            assert time.monotonic() - start >= 1.5
                        elif outcome == "cancel":
                            assert output.strip() in ("cancel", "3"), (output, error)
                        elif outcome == "dismiss":
                            assert output.strip() == "2", (output, error)
                        else:
                            assert output.strip() != "1", (output, error)
                        print(
                            outcome,
                            "=>",
                            repr(output.strip()),
                            "exit",
                            notification.returncode,
                        )
                    except subprocess.TimeoutExpired:
                        assert outcome == "daemon-loss"
                        print(
                            "daemon-loss => no expiry; bridge watchdog must fail closed"
                        )
                finally:
                    if notification.poll() is None:
                        notification.kill()
                        notification.communicate()
        finally:
            if daemon.poll() is None:
                daemon.terminate()
            daemon.communicate(timeout=3)


if __name__ == "__main__":
    if "--private-bus" in sys.argv:
        probe()
    else:
        env = dict(os.environ)
        env.pop("DBUS_SESSION_BUS_ADDRESS", None)
        subprocess.run(
            ["dbus-run-session", "--", sys.executable, __file__, "--private-bus"],
            check=True,
            env=env,
        )
