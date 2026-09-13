#!/usr/bin/env python3
"""Harmless external-process boundary; never invokes desktop utilities."""

import json
import os
from pathlib import Path
import sys
import time

root = Path(os.environ["HA_MQTT_TEST_STATE"])
name = sys.argv[1]
args = sys.argv[2:]
mode = (root / "mode").read_text() if (root / "mode").exists() else ""
with (root / "calls").open("a") as out:
    out.write(json.dumps([name, *args]) + "\n")
if name == "amixer":
    assert args[:2] == ["-D", "pipewire"]
    assert args[3] == "Master"
    if mode == "audio-failure":
        sys.exit(1)
    if mode == "audio-hang":
        time.sleep(30)
    if mode == "query-gate" and args[2] == "get":
        (root / "query-waiting").touch()
        while not (root / "release-query").exists():
            time.sleep(0.05)
    if args[2] == "get":
        if mode == "unparseable":
            print("Simple mixer control 'Master',0\n  Capabilities: pvolume")
        else:
            state = (root / "state").read_text()
            print("Simple mixer control 'Master',0\n  Capabilities: pvolume pswitch")
            print(f"  Mono: Playback 37 [37%] [{'off' if state == '1' else 'on'}]")
    else:
        assert args[2] == "set" and args[4] in ("mute", "unmute")
        (root / "state").write_text("1" if args[4] == "mute" else "0")
elif name == "dunstify":
    if args == ["--capabilities"]:
        print("body\nactions" if mode != "no-actions" else "body")
    elif mode == "notification-failure":
        sys.exit(1)
    elif mode == "notification-hang":
        time.sleep(30)
    elif mode == "cancel":
        print("cancel")
    elif mode == "dismiss":
        print("2")
    elif mode == "unexpected":
        print("unexpected")
    elif mode == "early-expiry":
        print("1")
    else:
        time.sleep(int(args[args.index("--timeout") + 1]) / 1000)
        print("1")
elif name == "systemctl":
    assert args == ["poweroff"]
    (root / "poweroff").write_text("requested")
else:
    raise AssertionError(name)
