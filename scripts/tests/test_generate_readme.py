import importlib.util
from pathlib import Path
import json
import subprocess
import sys
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "generate_readme", Path(__file__).resolve().parents[1] / "generate-readme.py"
)
generator = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generator)


class GenerateReadmeTests(unittest.TestCase):
    def test_requested_columns_sorted_hosts_and_links(self):
        inventory = {
            "zeta": {
                "purpose": "Storage NAS",
                "channel": "stable",
                "role": "server",
                "os": "nixos",
            },
            "alpha": {
                "purpose": "Workstation",
                "board": "Example board",
                "cpu": "Example CPU",
                "ramGiB": 64,
                "gpu": "Example GPU",
                "channel": "unstable",
                "role": "desktop",
                "os": "nixos",
            },
        }
        source = "Intro\n<!-- BEGIN HOSTS -->\nold table\n<!-- END HOSTS -->\nFooter\n"
        result = generator.render_readme(source, inventory)
        self.assertEqual(
            result.splitlines()[3:7],
            [
                "| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |",
                "|---|---|---|---|---|---|---|---|---|",
                '| [alpha](./hosts/alpha/) | Workstation | Example board | Example CPU | 64 GiB | Example GPU | unstable | <span title="Desktop">&#x1F5A5;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |',
                '| [zeta](./hosts/zeta/) | Storage NAS | Unknown | Unknown | Unknown | Unknown | stable | <span title="Bare-metal server">&#x1F5C4;&#xFE0F;</span> | <span title="NixOS">&#x2744;&#xFE0F;</span> |',
            ],
        )
        self.assertTrue(result.startswith("Intro\n<!-- BEGIN HOSTS -->\n\n"))
        self.assertTrue(result.endswith("\n<!-- END HOSTS -->\nFooter\n"))
        self.assertEqual(generator.render_readme(result, inventory), result)

    def test_cli_updates_preserves_prose_and_checks_drift_without_writing(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            readme = root / "README.md"
            inventory = root / "inventory.json"
            inventory.write_text(
                json.dumps(
                    {
                        "test": {
                            "purpose": "Old purpose",
                            "channel": "stable",
                            "role": "server",
                            "os": "nixos",
                        }
                    }
                )
            )
            original = b"Intro\r\n<!-- BEGIN HOSTS -->\r\nold\r\n<!-- END HOSTS -->\r\nFooter\r\n"
            readme.write_bytes(original)
            command = [
                sys.executable,
                str(generator.__file__),
                "--readme",
                str(readme),
                "--inventory-json",
                str(inventory),
            ]
            check = subprocess.run(
                command + ["--check"], capture_output=True, text=True
            )
            self.assertEqual((check.returncode, readme.read_bytes()), (1, original))
            self.assertIn("stale", check.stderr)
            update = subprocess.run(command, capture_output=True, text=True)
            self.assertEqual(update.returncode, 0, update.stderr)
            generated = readme.read_bytes()
            self.assertTrue(generated.startswith(b"Intro\r\n<!-- BEGIN HOSTS -->"))
            self.assertTrue(generated.endswith(b"<!-- END HOSTS -->\r\nFooter\r\n"))
            self.assertNotEqual(generated, original)
            check = subprocess.run(
                command + ["--check"], capture_output=True, text=True
            )
            self.assertEqual((check.returncode, readme.read_bytes()), (0, generated))
            changed = json.loads(inventory.read_text())
            changed["test"]["purpose"] = "New purpose"
            inventory.write_text(json.dumps(changed))
            check = subprocess.run(
                command + ["--check"], capture_output=True, text=True
            )
            self.assertEqual((check.returncode, readme.read_bytes()), (1, generated))

    def test_bad_markers_leave_readme_untouched(self):
        sources = [
            "No markers",
            "<!-- END HOSTS -->\n<!-- BEGIN HOSTS -->",
            "<!-- BEGIN HOSTS --><!-- BEGIN HOSTS --><!-- END HOSTS -->",
            "<!-- END HOSTS --><!-- BEGIN HOSTS --><!-- END HOSTS -->",
        ]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            inventory = root / "inventory.json"
            inventory.write_text("{}")
            for source in sources:
                with self.subTest(source=source):
                    readme = root / "README.md"
                    readme.write_text(source)
                    result = subprocess.run(
                        [
                            sys.executable,
                            generator.__file__,
                            "--readme",
                            str(readme),
                            "--inventory-json",
                            str(inventory),
                        ],
                        capture_output=True,
                        text=True,
                    )
                    self.assertEqual(
                        (result.returncode, readme.read_text()), (1, source)
                    )
                    self.assertIn("markers", result.stderr)

    def test_markdown_cells_are_literal_and_documentation_links_survive(self):
        host = {
            "purpose": "A | B\n<script>[link]*",
            "documentation": "https://example.org/docs_(host)",
            "channel": "stable",
            "role": "server",
            "os": "nixos",
        }
        result = generator.render_readme(
            "<!-- BEGIN HOSTS -->\n<!-- END HOSTS -->", {"example": host}
        )
        self.assertIn(
            r"[A &#124; B &lt;script&gt;\[link\]\*](https://example.org/docs_%28host%29)",
            result,
        )

    def test_invalid_inventory_cannot_render_a_misleading_table(self):
        host = {"purpose": "Test", "channel": "stable", "role": "server", "os": "nixos"}
        for field, value in [
            ("channel", "stabel"),
            ("role", "sever"),
            ("os", "unknown"),
            ("ramGiB", -1),
            ("documentation", "javascript:alert(1)"),
        ]:
            with self.subTest(field=field):
                with self.assertRaises(ValueError):
                    generator.render_readme(
                        "<!-- BEGIN HOSTS -->\n<!-- END HOSTS -->",
                        {"example": host | {field: value}},
                    )
        with self.assertRaises(ValueError):
            generator.render_readme(
                "<!-- BEGIN HOSTS -->\n<!-- END HOSTS -->", {"../escape": host}
            )


if __name__ == "__main__":
    unittest.main()
