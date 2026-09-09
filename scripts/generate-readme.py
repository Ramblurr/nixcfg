#!/usr/bin/env python3
"""Render the public host inventory without evaluating host configurations."""

import argparse
import html
import re
import json
from pathlib import Path
import subprocess
import sys
from urllib.parse import quote

BEGIN = "<!-- BEGIN HOSTS -->"
END = "<!-- END HOSTS -->"
# ASCII entities keep source free of emoji while rendering the requested icons.
ROLES = {
    "desktop": ("Desktop", "&#x1F5A5;&#xFE0F;"),
    "laptop": ("Laptop", "&#x1F4BB;&#xFE0F;"),
    "gaming": ("Games Machine", "&#x1F3AE;&#xFE0F;"),
    "vm": ("Virtual Machine", "&#x1F404;&#xFE0F;"),
    "cloud": ("Cloud Server", "&#x2601;&#xFE0F;"),
    "server": ("Bare-metal server", "&#x1F5C4;&#xFE0F;"),
    "inactive": ("Not in service", "&#x1F9DF;&#xFE0F;"),
}
OPERATING_SYSTEMS = {"nixos": ("NixOS", "&#x2744;&#xFE0F;")}


def cell(value):
    if value is None:
        return "Unknown"
    text = html.escape(" ".join(str(value).splitlines()), quote=False)
    return re.sub(r"([\\`*_\[\]])", r"\\\1", text).replace("|", "&#124;")


def icon(entry):
    label, entity = entry
    return f'<span title="{label}">{entity}</span>'


def render_readme(source, inventory):
    if (
        source.count(BEGIN) != 1
        or source.count(END) != 1
        or source.index(BEGIN) > source.index(END)
    ):
        raise ValueError(
            "README must contain exactly one ordered pair of host table markers"
        )
    before, rest = source.split(BEGIN)
    _, after = rest.split(END)
    rows = [
        "| Hostname | Purpose | Board | CPU | RAM | GPU | Channel | Role | OS |",
        "|---|---|---|---|---|---|---|---|---|",
    ]
    for name, host in sorted(inventory.items()):
        if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", name):
            raise ValueError(f"Invalid inventory hostname: {name}")
        for field, choices in [
            ("channel", ("stable", "unstable")),
            ("role", ROLES),
            ("os", OPERATING_SYSTEMS),
        ]:
            if not isinstance(host.get(field), str) or host[field] not in choices:
                raise ValueError(f"{name}: invalid {field}: {host.get(field)!r}")
        ram = host.get("ramGiB")
        if ram is not None and (type(ram) is not int or ram <= 0):
            raise ValueError(f"{name}: ramGiB must be a positive integer or null")
        purpose = cell(host["purpose"])
        documentation = host.get("documentation")
        if documentation is not None:
            if not documentation.startswith("https://"):
                raise ValueError(f"{name}: documentation must be an HTTPS URL")
            purpose = f"[{purpose}]({quote(documentation, safe=':/#?=&%')})"
        values = [
            f"[{name}](./hosts/{name}/)",
            purpose,
            cell(host.get("board")),
            cell(host.get("cpu")),
            cell(f"{ram} GiB" if ram is not None else None),
            cell(host.get("gpu")),
            cell(host["channel"]),
            icon(ROLES[host["role"]]),
            icon(OPERATING_SYSTEMS[host["os"]]),
        ]
        rows.append("| " + " | ".join(values) + " |")
    rows.extend(["", "**Roles**", ""])
    rows.extend(f"- {entity}: {label}" for label, entity in ROLES.values())
    rows.extend(["", "**OS**", ""])
    rows.extend(
        f"- {entity}: {label}"
        for key, (label, entity) in OPERATING_SYSTEMS.items()
        if any(host["os"] == key for host in inventory.values())
    )
    return before + BEGIN + "\n\n" + "\n".join(rows) + "\n\n" + END + after


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check", action="store_true", help="Fail if the table is stale; do not write"
    )
    parser.add_argument("--readme", type=Path, default=root / "README.md")
    parser.add_argument(
        "--inventory-json",
        type=Path,
        help="Use an already evaluated public inventory (for CI)",
    )
    args = parser.parse_args()
    try:
        if args.inventory_json:
            inventory = json.loads(args.inventory_json.read_text(encoding="utf-8"))
        else:
            inventory = json.loads(
                subprocess.check_output(
                    [
                        "nix",
                        "eval",
                        "--json",
                        "--file",
                        str(root / "hosts/inventory.nix"),
                    ],
                    text=True,
                )
            )
        original = args.readme.read_bytes()
        rendered = render_readme(original.decode("utf-8"), inventory).encode("utf-8")
        if rendered != original:
            if args.check:
                print(
                    f"{args.readme}: host table is stale; run python3 scripts/generate-readme.py",
                    file=sys.stderr,
                )
                return 1
            args.readme.write_bytes(rendered)
            print(f"Updated {args.readme}")
        return 0
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        print(f"generate-readme: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
