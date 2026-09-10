"""Exercise wrapper command boundaries without touching live systems."""
import json
import os
from pathlib import Path
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
            result[name].update(host="host", guestIP="192.0.2.23" if name == "guest" else "192.0.2.24")
    print(json.dumps(result))
elif command in ("nom", "nix") and args[0] == "build":
    if os.environ.get("FAIL_BUILD"):
        sys.exit(41)
    print(os.environ["SYSTEM"])
elif command == "microvm-rebuild":
    sys.exit(42 if os.environ.get("FAIL_GUEST") else 0)
elif command == "nix" and args[0] == "copy":
    pass
elif command == "ssh":
    if "readlink" in args:
        print("/previous-system")
else:
    sys.exit(64)
'''


class Wrappers(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        (self.root / "flake.nix").touch()
        self.log = self.root / "commands.jsonl"
        for name in ("git", "nix", "nom", "ssh", "microvm-rebuild"):
            path = self.bin / name
            path.write_text(f"#!{sys.executable}\n" + STUB)
            path.chmod(0o755)
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        ROOT=str(self.root), SYSTEM=str(self.root), LOG=str(self.log))

    def run_wrapper(self, executable, *args, success=True):
        result = subprocess.run([executable, *args], cwd=self.root, env=self.env,
                                capture_output=True, text=True)
        self.assertEqual(result.returncode == 0, success, result.stdout + result.stderr)
        self.stdout = result.stdout
        return [json.loads(line) for line in self.log.read_text().splitlines()]

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
        self.assertIn("Hosts\n  host\n  zhost\n\nGuests    Runs on\n  guest   host\n  guest2  host\n", outputs[0])
        for phrase in ("Usage: build <name>...", "deploy [OPTIONS] <name,...> [ACTION]",
                       "Deploy actions", "switch", "boot", "test", "dry-activate",
                       "Guests support switch only.", "Options", "--show-trace, --keep-going, --verbose",
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
        self.assertEqual(commands[-1], ["nom", "build", "--no-link", "--print-out-paths", "--show-trace",
                         ".#nixosConfigurations.host.config.system.build.toplevel",
                         ".#nixosConfigurations.guest.config.microvm.declaredRunner"])
        self.assertFalse(any(c[0] == "ssh" for c in commands))

    def test_switch_default_and_explicit(self):
        for action in ([], ["switch"]):
            with self.subTest(action=action):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, "--show-trace", "guest", *action)
                self.assertEqual(commands[-1], ["microvm-rebuild", "root@host", "root@192.0.2.23"])
                self.assertIn(["nom", "build", "--no-link", "--print-out-paths", "--show-trace",
                               ".#nixosConfigurations.guest.config.microvm.deploy.rebuild"], commands)
                self.assertFalse(any(c[0] == "ssh" or c[:2] == ["nix", "copy"] for c in commands))

    def test_mixed_deploy(self):
        commands = self.run_wrapper(DEPLOY, "host,guest,guest2")
        self.assertIn(["microvm-rebuild", "root@host", "root@192.0.2.24"], commands)
        copies = [c for c in commands if c[:2] == ["nix", "copy"]]
        self.assertEqual(len(copies), 1)
        self.assertIn("ssh://root@host", copies[0])
        self.assertTrue(any(c[0] == "ssh" and "switch" in c for c in commands))

    def test_unsupported_actions_fail_before_build_or_activation(self):
        for action in ("boot", "test", "dry-activate", "invalid"):
            with self.subTest(action=action):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, "host,guest", action, success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_unknown_and_invalid_names(self):
        for name in ("unknown", "host,unknown", "guest.bad", "host,"):
            with self.subTest(name=name):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(DEPLOY, name, success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_metadata_failure_blocks_build_and_deploy(self):
        self.env["FAIL_METADATA"] = "1"
        for executable, names in ((BUILD, ["host", "guest"]), (DEPLOY, ["host,guest"])):
            with self.subTest(executable=executable):
                self.log.unlink(missing_ok=True)
                commands = self.run_wrapper(executable, *names, success=False)
                self.assertTrue(all(c[0] == "git" or c[:2] == ["nix", "eval"] for c in commands))

    def test_failures_propagate(self):
        self.env["FAIL_BUILD"] = "1"
        commands = self.run_wrapper(DEPLOY, "guest", success=False)
        self.assertFalse(any(c[0] == "microvm-rebuild" for c in commands))
        del self.env["FAIL_BUILD"]
        self.env["FAIL_GUEST"] = "1"
        self.run_wrapper(DEPLOY, "guest", success=False)

    def test_nix_fallback(self):
        (self.bin / "nom").unlink()
        commands = self.run_wrapper(BUILD, "guest")
        self.assertEqual(commands[-1][0:2], ["nix", "build"])
        self.log.unlink()
        commands = self.run_wrapper(DEPLOY, "guest")
        self.assertTrue(any(c[:2] == ["nix", "build"] for c in commands))
        self.assertEqual(commands[-1], ["microvm-rebuild", "root@host", "root@192.0.2.23"])

    def test_host_boot(self):
        commands = self.run_wrapper(DEPLOY, "host", "boot")
        self.assertTrue(any(c[0] == "ssh" and "boot" in c for c in commands))
        self.assertFalse(any(c[0] == "microvm-rebuild" for c in commands))


unittest.main()
