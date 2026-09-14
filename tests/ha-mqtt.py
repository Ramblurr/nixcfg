#!/usr/bin/env python3
"""Disposable MQTT seam: production bridge/helpers, real MQTTX, no live desktop."""

import json
import os
from pathlib import Path
import signal
import socket
import subprocess
import tempfile
import time
import unittest

BRIDGE = os.environ["HA_MQTT_BRIDGE"]


def wait_for(predicate, timeout=12):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if predicate():
            return
        time.sleep(0.05)
    raise AssertionError("Timed out waiting for observable result")


class BridgeTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="ha-mqtt-test-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.children = []
        self.event_fds = {}
        self.addCleanup(self.stop_all)
        with socket.socket() as sock:
            sock.bind(("127.0.0.1", 0))
            self.port = sock.getsockname()[1]
        (self.root / "broker.conf").write_text(
            f"listener {self.port} 127.0.0.1\nallow_anonymous true\npersistence false\n"
        )
        self.broker = self.start_broker()
        self.state = self.root / "a"
        self.bridge = self.start_bridge("a")

    def spawn(self, args, log, env=None):
        with log.open("ab") as out:
            child = subprocess.Popen(
                args, stdout=out, stderr=out, env=env, start_new_session=True
            )
        self.children.append(child)
        return child

    def stop(self, child):
        if child.poll() is None:
            os.killpg(child.pid, signal.SIGTERM)
            try:
                child.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(child.pid, signal.SIGKILL)
                child.wait()

    def stop_all(self):
        for child in reversed(self.children):
            self.stop(child)

    def start_broker(self):
        child = self.spawn(
            ["mosquitto", "-v", "-c", str(self.root / "broker.conf")],
            self.root / "broker.log",
        )
        wait_for(lambda: "running" in (self.root / "broker.log").read_text())
        time.sleep(0.2)
        return child

    def start_bridge(self, name, enabled=True, dry_run=False):
        root = self.root / name
        root.mkdir(exist_ok=True)
        (root / "state").write_text("0")
        if not (root / "events").exists():
            os.mkfifo(root / "events")
        if name not in self.event_fds:
            self.event_fds[name] = os.open(root / "events", os.O_RDWR)
            self.addCleanup(os.close, self.event_fds[name])
        credentials = root / "password"
        credentials.write_text("fixture-password\n")
        credentials.chmod(0o600)
        config = root / "config.json"
        config.write_text(
            json.dumps(
                {
                    "topicPrefix": f"ha-mqtt/{name}",
                    "mqtt": {
                        "host": "127.0.0.1",
                        "port": self.port,
                        "tls": {"enable": False},
                        "username": "fixture-user",
                        "passwordFile": str(credentials),
                    },
                    "shutdown": {
                        "enable": enabled,
                        "gracePeriodMs": 1500,
                        "dryRun": dry_run,
                    },
                }
            )
        )
        env = dict(os.environ, HA_MQTT_TEST_STATE=str(root), TMPDIR=str(root))
        # No real desktop bus or audio runtime is accessible to the test process.
        env.pop("DBUS_SESSION_BUS_ADDRESS", None)
        env.pop("XDG_RUNTIME_DIR", None)
        subscriptions = (
            (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
        )
        child = self.spawn(["bb", BRIDGE, str(config)], root / "bridge.log", env)
        wait_for(
            lambda: (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
            > subscriptions
        )
        wait_for(lambda: "speaker-monitor-started" in self.log(name))
        wait_for(lambda: self.log(name).count("speaker-state-published") >= 2)
        self.assertEqual("0", self.observed(name))
        return child

    def log(self, name="a"):
        return (self.root / name / "bridge.log").read_text()

    def audio_event(self, event="Event 'change' on sink #1", name="a"):
        os.write(self.event_fds[name], (event + "\n").encode())

    def publish(self, payload, name="a", retain=False):
        subprocess.run(
            [
                "mosquitto_pub",
                "-h",
                "127.0.0.1",
                "-p",
                str(self.port),
                "-t",
                f"ha-mqtt/{name}/command",
                "-m",
                payload,
                *(["-r"] if retain else []),
            ],
            check=True,
            capture_output=True,
            timeout=5,
        )

    def observed(self, name="a"):
        result = subprocess.run(
            [
                "mosquitto_sub",
                "-h",
                "127.0.0.1",
                "-p",
                str(self.port),
                "-t",
                f"ha-mqtt/{name}/speaker/muted",
                "-C",
                "1",
                "-W",
                "2",
            ],
            check=True,
            capture_output=True,
            text=True,
            timeout=4,
        )
        return result.stdout.strip()

    def action(self, payload, state):
        before = self.log().count("speaker-state-published")
        self.publish(payload)
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual(state, self.observed())

    def calls(self):
        return (self.state / "calls").read_text()

    def mode(self, value):
        (self.state / "mode").write_text(value)

    def test_local_mute_and_default_output_changes(self):
        before = self.log().count("speaker-state-published")
        (self.state / "state").write_text("1")
        self.audio_event()
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual("1", self.observed())
        before = self.log().count("speaker-state-published")
        self.audio_event()
        self.audio_event("Event 'change' on sink-input #2")
        self.audio_event("Event 'new' on client #3")
        time.sleep(0.5)
        self.assertEqual(before, self.log().count("speaker-state-published"))
        (self.state / "state").write_text("0")
        self.audio_event("Event 'change' on server #4294967295")
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual("0", self.observed())
        self.assertNotIn('"set"', self.calls())

    def test_monitor_death_refreshes_state_and_resumes_events(self):
        pid = int((self.state / "monitor-pid").read_text())
        os.kill(pid, signal.SIGKILL)
        (self.state / "state").write_text("1")
        wait_for(lambda: int((self.state / "monitor-pid").read_text()) != pid)
        wait_for(lambda: self.log().count("speaker-monitor-started") >= 2)
        self.assertEqual("1", self.observed())
        before = self.log().count("speaker-state-published")
        (self.state / "state").write_text("0")
        self.audio_event()
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual("0", self.observed())

    def test_local_query_failure_recovers_without_fabricating_state(self):
        self.mode("audio-failure")
        before = self.log().count("speaker-state-published")
        (self.state / "state").write_text("1")
        self.audio_event()
        wait_for(lambda: "speaker-monitor-failed" in self.log())
        self.assertEqual(before, self.log().count("speaker-state-published"))
        self.assertEqual("0", self.observed())
        self.mode("")
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual("1", self.observed())

    def test_speakers_and_host_isolation(self):
        self.start_bridge("override")
        for action, state in [
            ("speaker-mute", "1"),
            ("speaker-mute", "1"),
            ("speaker-get-mute", "1"),
            ("speaker-unmute", "0"),
            ("speaker-unmute", "0"),
            ("speaker-toggle", "1"),
            ("speaker-toggle", "0"),
        ]:
            self.action(action, state)
            self.assertEqual("0", self.observed("override"))
        self.assertNotIn("fixture-password", self.log())
        self.assertNotIn("fixture-user", self.log())
        pending = [child.pid for child in self.children]
        while pending:
            pid = pending.pop()
            try:
                argv = Path(f"/proc/{pid}/cmdline").read_bytes()
                pending.extend(
                    int(child)
                    for child in Path(f"/proc/{pid}/task/{pid}/children")
                    .read_text()
                    .split()
                )
            except (FileNotFoundError, PermissionError, ProcessLookupError):
                continue
            self.assertNotIn(b"fixture-password", argv)
            self.assertNotIn(b"fixture-user", argv)

    def test_invalid_messages_and_retained_live_delivery(self):
        # Readiness may have issued several harmless queries; let them drain.
        time.sleep(0.5)
        before = self.calls()
        for payload in [
            "",
            "unknown",
            "speaker-mute x",
            "speaker-mute\nspeaker-toggle",
            "$(systemctl poweroff)",
            "shutdown; true",
            " speaker-mute",
            "x" * 500,
        ]:
            self.publish(payload)
        self.publish("shutdown", retain=True)
        self.publish("speaker-toggle", retain=True)
        wait_for(lambda: self.log().count("command-rejected") >= 10)
        self.assertEqual(before, self.calls())
        self.assertFalse((self.state / "poweroff").exists())
        self.action("speaker-mute", "1")

    def test_failed_queries_do_not_toggle_or_fabricate_state(self):
        self.action("speaker-mute", "1")
        for mode in ["audio-failure", "unparseable"]:
            self.mode(mode)
            before = self.log().count("speaker-action-or-publish-failed")
            self.publish("speaker-toggle")
            wait_for(
                lambda: self.log().count("speaker-action-or-publish-failed") > before
            )
            self.assertEqual("1", self.observed())
            self.assertEqual("1", (self.state / "state").read_text())
        self.mode("")
        self.action("speaker-unmute", "0")

    def test_restart_drops_retained_and_offline_commands(self):
        self.stop(self.bridge)
        self.publish("shutdown", retain=True)
        self.publish("speaker-toggle")
        self.bridge = self.start_bridge("a")
        self.action("speaker-get-mute", "0")
        self.assertFalse((self.state / "poweroff").exists())
        self.stop(self.bridge)
        self.publish("speaker-toggle", retain=True)
        self.publish("shutdown")
        self.bridge = self.start_bridge("a")
        self.action("speaker-get-mute", "0")
        self.assertFalse((self.state / "poweroff").exists())

    def test_oversized_command_never_executes(self):
        before = self.calls()
        self.publish("shutdown" + "x" * 10000)
        self.action("speaker-get-mute", "0")
        self.assertEqual(
            before + '["amixer", "-D", "pipewire", "get", "Master"]\n', self.calls()
        )
        self.action("speaker-mute", "1")

    def test_runtime_password_permissions_and_publisher_exit_status(self):
        configs = list(self.state.glob("ha-mqtt-*/mqttx.json"))
        self.assertEqual(1, len(configs))
        self.assertEqual(0o600, configs[0].stat().st_mode & 0o777)
        self.assertEqual(0o700, configs[0].parent.stat().st_mode & 0o777)
        self.assertNotIn("fixture-password", (self.state / "config.json").read_text())
        self.stop(self.bridge)
        self.assertFalse(configs[0].exists())
        (self.state / "password").chmod(0o644)
        result = subprocess.run(
            ["bb", BRIDGE, str(self.state / "config.json")],
            capture_output=True,
            text=True,
            timeout=5,
        )
        self.assertNotEqual(0, result.returncode)
        self.assertNotIn("fixture-password", result.stdout + result.stderr)
        refused = subprocess.run(
            [
                "mqttx-cli",
                "pub",
                "-h",
                "127.0.0.1",
                "-p",
                "1",
                "-t",
                "ha-mqtt/test/state",
                "-m",
                "0",
                "-q",
                "1",
                "-rp",
                "0",
            ],
            capture_output=True,
            text=True,
            timeout=5,
        )
        self.assertNotEqual(0, refused.returncode)

    def test_broker_failure_during_state_publish_and_recovery(self):
        self.action("speaker-mute", "1")
        self.mode("query-gate")
        before = self.log().count("speaker-state-published")
        failures = self.log().count("speaker-action-or-publish-failed")
        self.publish("speaker-get-mute")
        wait_for(lambda: (self.state / "query-waiting").exists())
        self.stop(self.broker)
        (self.state / "release-query").touch()
        wait_for(
            lambda: self.log().count("speaker-action-or-publish-failed") > failures
        )
        self.assertEqual(before, self.log().count("speaker-state-published"))
        self.mode("")
        subscriptions = (
            (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
        )
        self.broker = self.start_broker()
        self.publish("shutdown", retain=True)
        self.publish("speaker-toggle")
        wait_for(
            lambda: (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
            > subscriptions
        )
        wait_for(lambda: self.log().count("speaker-state-published") > before)
        self.assertEqual("1", self.observed())
        self.action("speaker-get-mute", "1")
        self.assertFalse((self.state / "poweroff").exists())

    def test_subscriber_death_recovers_without_retained_replay(self):
        self.publish("speaker-toggle", retain=True)
        wait_for(lambda: "command-rejected" in self.log())
        children = (
            Path(f"/proc/{self.bridge.pid}/task/{self.bridge.pid}/children")
            .read_text()
            .split()
        )
        subscribers = [
            int(pid)
            for pid in children
            if b"sub\0" in Path(f"/proc/{pid}/cmdline").read_bytes()
        ]
        self.assertEqual(1, len(subscribers))
        subscriptions = (
            (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
        )
        os.kill(subscribers[0], signal.SIGTERM)
        wait_for(
            lambda: (self.root / "broker.log")
            .read_text()
            .count("Received SUBSCRIBE from ha-mqtt-sub-")
            > subscriptions
        )
        self.action("speaker-get-mute", "0")

    def test_hung_actions_and_notifications_are_bounded(self):
        self.mode("audio-hang")
        self.publish("speaker-toggle")
        wait_for(lambda: "speaker-action-or-publish-failed" in self.log())
        self.mode("notification-hang")
        self.publish("shutdown")
        wait_for(lambda: "shutdown-failed" in self.log())
        self.assertFalse((self.state / "poweroff").exists())
        self.mode("")
        self.action("speaker-mute", "1")

    def test_shutdown_fail_safe_and_expiry(self):
        for mode in [
            "cancel",
            "dismiss",
            "unexpected",
            "early-expiry",
            "notification-failure",
            "no-actions",
        ]:
            self.mode(mode)
            before = self.log().count("shutdown-cancelled") + self.log().count(
                "shutdown-failed"
            )
            self.publish("shutdown")
            wait_for(
                lambda: self.log().count("shutdown-cancelled")
                + self.log().count("shutdown-failed")
                > before
            )
            self.assertFalse((self.state / "poweroff").exists(), mode)
        self.mode("")
        self.publish("shutdown")
        wait_for(lambda: self.log().count("shutdown-pending") >= 6)
        self.publish("shutdown")
        self.action("speaker-mute", "1")
        wait_for(lambda: (self.state / "poweroff").exists())
        self.assertIn("shutdown-already-pending", self.log())
        self.assertEqual(1, self.calls().count('["systemctl", "poweroff"]'))

    def test_service_stop_cancels_pending(self):
        self.mode("notification-hang")
        self.publish("shutdown")
        wait_for(lambda: "shutdown-pending" in self.log())
        monitor_pid = int((self.state / "monitor-pid").read_text())
        # Stop only Babashka: its own shutdown hook must clean up the children.
        self.bridge.terminate()
        self.bridge.wait(timeout=5)
        time.sleep(2)
        self.assertFalse((self.state / "poweroff").exists())
        self.assertFalse(Path(f"/proc/{monitor_pid}").exists())

    def test_shutdown_disabled_and_dry_run(self):
        self.stop(self.bridge)
        self.bridge = self.start_bridge("a", enabled=False)
        self.publish("shutdown")
        wait_for(lambda: "shutdown-disabled" in self.log())
        self.assertFalse((self.state / "poweroff").exists())
        self.stop(self.bridge)
        self.bridge = self.start_bridge("a", dry_run=True)
        self.publish("shutdown")
        wait_for(lambda: "shutdown-dry-run" in self.log())
        self.assertFalse((self.state / "poweroff").exists())


if __name__ == "__main__":
    unittest.main()
