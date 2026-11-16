"""Resolve 1Password references at runtime, then atomically write a private config."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def render(value, reader, files):
    if isinstance(value, dict):
        return {key: render(item, reader, files) for key, item in value.items()}
    if isinstance(value, list):
        return [render(item, reader, files) for item in value]
    if isinstance(value, str) and value.startswith("op://"):
        if value in files:
            secret = Path(files[value]).read_text().strip()
        else:
            result = subprocess.run([reader, value], capture_output=True, text=True)
            if result.returncode:
                raise RuntimeError(f"Cannot resolve {value}")
            secret = result.stdout.strip()
        if not secret:
            raise RuntimeError(f"Empty credential: {value}")
        return secret
    return value


def main():
    template, reader, output = sys.argv[1:]
    definition = json.loads(Path(template).read_text())
    settings = render(definition["settings"], reader, definition["secretFiles"])
    destination = Path(output)
    fd, temporary = tempfile.mkstemp(dir=destination.parent, prefix=".config-")
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(settings, stream)
            stream.write("\n")
        os.replace(temporary, destination)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


if __name__ == "__main__":
    try:
        main()
    except (OSError, RuntimeError, ValueError) as error:
        # Never expose command output or resolved settings in service logs.
        print(f"CLIProxyAPI config generation failed: {type(error).__name__}", file=sys.stderr)
        sys.exit(1)
