"""Exercise wrapper command boundaries without touching live systems."""
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import unittest

BUILD, DEPLOY = sys.argv[1:]
sys.argv[1:] = []

STUB = r'''
import json, os, re, sys
from pathlib import Path
command = Path(sys.argv[0]).name
args = sys.argv[1:]
with open(os.environ["LOG"], "a") as log:
    log.write(json.dumps([command, *args]) + "\n")
if command == "git":
    print(os.environ["ROOT"])
elif command == "nix" and args[0] == "eval":
    if os.environ.get("FAIL_METADATA"):
        sys.exit(40)
    expression = args[args.index("--apply") + 1]
    if "names = builtins.attrNames cs" in expression:
        names = [] if os.environ.get("EMPTY_INVENTORY") else ["guest2", "zhost", "guest", "host"]
    else:
        names = re.findall(r'"([a-zA-Z0-9_-]+)"', expression.split("names = [", 1)[1].split("]", 1)[0])
    result = {}
    for name in names:
        if name not in ("host", "zhost", "guest", "guest2"):
            sys.exit(1)
        guest = name.startswith("guest")
        result[name] = {"guest": guest, "buildAttribute": "nixosConfigurations." + name + ".config." + ("microvm.declaredRunner" if guest else "system.build.toplevel")}
        if guest:
            result[name].update(host="host", activation="hot" if name == "guest" else "restart",
                                guestSSH="vsock-mux//var/lib/microvms/guest/notify.vsock" if name == "guest" else "vsock/4244")
    print(json.dumps(result))
elif command in ("nom", "nix") and args[0] == "build":
    if os.environ.get("FAIL_BUILD"):
        sys.exit(41)
    for a in args:
        if a.endswith(".config.system.build.toplevel"):
            print("/nix/store/" + "c" * 32 + "-nixos-system-" + a.split(".")[2])
        elif a.startswith(".#"):
            print(os.environ["SYSTEM"] + "/" + a.split(".config.", 1)[1])
elif command == "nix" and args[0] == "copy":
    sys.exit(46 if os.environ.get("FAIL_COPY") else 0)
elif command == "microvm-install-on-host":
    sys.exit(45 if os.environ.get("FAIL_INSTALL") else 0)
elif command == "ssh":
    remote = " ".join(args[args.index("--") + 1:]) if "--" in args else ""
    if "microvm-switch" in remote and os.environ.get("FAIL_SWITCH"):
        sys.exit(44)
    if remote.startswith("nix-store --check-validity") and os.environ.get("FAIL_VALID"):
        sys.exit(1)
    if "readlink -e /run/current-system" in remote:
        print(os.environ.get("RUNNING", "/nix/store/" + "a" * 32 + "-nixos-system-guest-1"))
    if "readlink" in args:
        print("/previous-system")
else:
    sys.exit(64)
'''


def guest_steps(commands):
    "The guest operations deploy ran, in order."
    steps = []
    for c in commands:
        if c[0] == "microvm-install-on-host":
            steps.append("install")
        elif c[0] == "ssh" and "--" in c:
            remote = shlex.split(" ".join(c[c.index("--") + 1:]))
            if remote[0].endswith("/bin/microvm-switch"):
                steps.append("activate:" + remote[2])
            elif remote[:2] == ["systemctl", "restart"]:
                steps.append("restart")
            elif remote[0] == "nvd" and "-nixos-system-guest" in remote[-1]:
                steps.append("diff")
    return steps


class Wrappers(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        (self.root / "flake.nix").touch()
        self.log = self.root / "commands.jsonl"
        installer = self.root / "microvm.deploy.installOnHost" / "bin"
        installer.mkdir(parents=True)
        for path in [self.bin / name for name in ("git", "nix", "nom", "ssh")] + [installer / "microvm-install-on-host"]:
            path.write_text(f"#!{sys.executable}\n" + STUB)
            path.chmod(0o755)
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        ROOT=str(self.root), SYSTEM=str(self.root), LOG=str(self.log))

    def run_wrapper(self, executable, *args, success=True):
        result = subprocess.run([executable, *args], cwd=self.root, env=self.env,
                                capture_output=True, text=True)
        self.assertEqual(result.returncode == 0, success, result.stdout + result.stderr)
        self.stdout = result.stdout
        self.stderr = result.stderr
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def test_guest_action_plans(self):
        plans = {
            ("guest", "diff"): ["diff"],
            ("guest", "dry-activate"): ["activate:dry-activate"],
            ("guest", "test"): ["activate:test"],
            ("guest", "boot"): ["activate:boot", "install"],
            ("guest", "switch"): ["activate:switch", "install"],
            ("guest2", "diff"): ["diff"],
            ("guest2", "boot"): ["install"],
            ("guest2", "switch"): ["install", "restart"],
        }
        for (guest, action), steps in plans.items():
            with self.subTest(guest=guest, action=action):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, action, guest)
                self.assertEqual(guest_steps(commands), steps)
                target = "vsock-mux//var/lib/microvms/guest/notify.vsock" if guest == "guest" else "vsock/4244"
                for c in commands:
                    if c[0] == "ssh" and "microvm-switch" in c[-1]:
                        self.assertEqual(c[1:3], ["root@host", "--"])
                        self.assertEqual(shlex.split(c[-1])[1], "root@" + target)
                self.assertFalse(any("192.0.2." in str(c) for c in commands))

    def test_guest_refusals_happen_before_build(self):
        for action in ("test", "dry-activate"):
            with self.subTest(action=action):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, action, "guest", "guest2", success=False)
                self.assertIn(f"error: Guest guest2 cannot {action}: ", self.stderr)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_failed_activation_leaves_the_runner(self):
        self.env["FAIL_SWITCH"] = "1"
        commands = self.run_wrapper(DEPLOY, "switch", "guest", success=False)
        self.assertEqual(guest_steps(commands), ["activate:switch"])
        self.assertIn("runner on host is unchanged", self.stderr)

    def test_failures_before_activation_stop_the_guest(self):
        for failure in ("FAIL_BUILD", "FAIL_COPY"):
            with self.subTest(failure=failure):
                self.log.unlink(missing_ok=True)
                self.env[failure] = "1"
                commands = self.run_wrapper(DEPLOY, "switch", "guest", success=False)
                del self.env[failure]
                self.assertEqual(guest_steps(commands), [])

    def test_restart_guest_already_running_the_build_is_left_alone(self):
        self.env["RUNNING"] = "/nix/store/" + "c" * 32 + "-nixos-system-guest2"
        commands = self.run_wrapper(DEPLOY, "switch", "guest2")
        self.assertEqual(guest_steps(commands), ["install"])
        self.assertIn("guest2 already runs", self.stdout)

    def test_guest_running_system_is_validated(self):
        self.env["RUNNING"] = "/tmp/evil; reboot"
        commands = self.run_wrapper(DEPLOY, "diff", "guest", success=False)
        self.assertIn("Unexpected running system for guest", self.stderr)
        self.assertFalse(any(c[0] == "ssh" and "nix-store" in c for c in commands))

    def test_guest_diff_needs_the_running_system_on_the_host(self):
        self.env["FAIL_VALID"] = "1"
        commands = self.run_wrapper(DEPLOY, "diff", "guest", success=False)
        self.assertEqual(guest_steps(commands), [])
        self.assertIn("store no longer holds", self.stderr)

    def test_no_args_and_help(self):
        outputs = []
        for executable, args in ((BUILD, []), (DEPLOY, []), (DEPLOY, ["--help"])):
            with self.subTest(executable=executable, args=args):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(executable, *args)
                self.assertEqual([c[0] for c in commands], ["git", "nix"])
                self.assertEqual(commands[-1][1], "eval")
                outputs.append(self.stdout)
        self.assertEqual(outputs, [outputs[0]] * 3)
        for args in (["switch"], ["--show-trace", "diff"]):
            with self.subTest(args=args):
                self.run_wrapper(DEPLOY, *args, success=False)
                self.assertEqual(self.stdout, outputs[0])
        self.assertIn("Hosts\n  host\n  zhost\n\nGuests    Runs on\n  guest   host\n  guest2  host\n", outputs[0])
        for phrase in ("Usage: build <name>...", "deploy [OPTIONS] <COMMAND> <name>...",
                       "Deploy commands", "switch", "boot", "test", "dry-activate", "diff",
                       "A guest without a hot switch (ward) refuses test and dry-activate,", "Options", "--show-trace, --keep-going, --verbose",
                       "Local build options do not apply to that remote build."):
            self.assertIn(phrase, outputs[0])

    def test_help_empty_inventory_and_eval_failure(self):
        for executable in (BUILD, DEPLOY):
            with self.subTest(executable=executable):
                self.env["EMPTY_INVENTORY"] = "1"
                self.run_wrapper(executable)
                self.assertIn("Hosts\n\nGuests    Runs on\n", self.stdout)
                del self.env["EMPTY_INVENTORY"]
                self.env["FAIL_METADATA"] = "1"
                self.run_wrapper(executable, success=False)
                del self.env["FAIL_METADATA"]


    def test_mixed_build(self):
        commands = self.run_wrapper(BUILD, "host", "guest")
        self.assertEqual(commands[-1], ["nom", "build", "--keep-going", "--no-link", "--print-out-paths", "--show-trace",
                         ".#nixosConfigurations.host.config.system.build.toplevel",
                         ".#nixosConfigurations.guest.config.microvm.declaredRunner"])
        self.assertFalse(any(c[0] == "ssh" for c in commands))

    def test_switch_requires_command_first(self):
        commands = self.run_wrapper(DEPLOY, "--show-trace", "switch", "guest")
        self.assertIn(["nom", "build", "--no-link", "--print-out-paths", "--show-trace",
                       ".#nixosConfigurations.guest.config.microvm.deploy.sshSwitch",
                       ".#nixosConfigurations.guest.config.microvm.deploy.installOnHost"], commands)
        for args in (["guest"], ["guest", "switch"]):
            with self.subTest(args=args):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, *args, success=False)
                self.assertIn("error: Unknown command: guest", self.stderr)
                self.assertTrue(all(c[0] == "git" for c in commands))

    def test_mixed_deploy(self):
        commands = self.run_wrapper(DEPLOY, "switch", "host", "guest", "guest2")
        copies = [c for c in commands if c[:2] == ["nix", "copy"]]
        self.assertEqual(len(copies), 3)
        self.assertFalse(any("installOnHost" in str(c) for c in copies))
        self.assertTrue(all("ssh://root@host" in c for c in copies))
        self.assertTrue(any(c[0] == "ssh" and c[-1] == "switch" and "switch-to-configuration" in c[-2] for c in commands))
        self.assertEqual(guest_steps(commands), ["activate:switch", "install", "install", "restart"])

    def test_unsupported_actions_fail_before_build_or_activation(self):
        for action in ("invalid", "reboot"):
            with self.subTest(action=action):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, action, "host", "guest", success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_unknown_and_invalid_names(self):
        for names in (["unknown"], ["host", "unknown"], ["guest.bad"], ["host,guest"]):
            with self.subTest(names=names):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, "switch", *names, success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_metadata_failure_blocks_build_and_deploy(self):
        self.env["FAIL_METADATA"] = "1"
        for executable, names in ((BUILD, ["host", "guest"]), (DEPLOY, ["switch", "host", "guest"])):
            with self.subTest(executable=executable):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(executable, *names, success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_nix_fallback(self):
        (self.bin / "nom").unlink()
        commands = self.run_wrapper(BUILD, "guest")
        self.assertEqual(commands[-1][0:2], ["nix", "build"])
        self.log.unlink()
        commands = self.run_wrapper(DEPLOY, "switch", "guest")
        self.assertTrue(any(c[:2] == ["nix", "build"] for c in commands))
        self.assertEqual(guest_steps(commands), ["activate:switch", "install"])

    def test_host_boot(self):
        commands = self.run_wrapper(DEPLOY, "boot", "host")
        self.assertTrue(any(c[0] == "ssh" and "boot" in c for c in commands))


unittest.main()
