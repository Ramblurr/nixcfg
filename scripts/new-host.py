import uuid
import subprocess
import tempfile
import sys
import os
import shutil
from pathlib import Path
from passlib.hash import sha512_crypt
import getpass
from ruamel.yaml import YAML
from ruamel.yaml.scalarstring import LiteralScalarString
from simple_term_menu import TerminalMenu
import textwrap


def LS(s):
    return LiteralScalarString(textwrap.dedent(s))


hostname = input("Enter HOSTNAME: ")

options = ["[x] x86_64-linux", "[a] aarch64-linux"]
terminal_menu = TerminalMenu(options, title="Arch")
menu_entry_index = terminal_menu.show()
if menu_entry_index == 0:
    arch = "x86_64-linux"
else:
    arch = "aarch64-linux"

options = ["[s] stable ", "[u] unstable"]
terminal_menu = TerminalMenu(options, title="Channel")
menu_entry_index = terminal_menu.show()
if menu_entry_index == 0:
    channel = "stable"
else:
    channel = "unstable"


host_path = Path("./hosts") / channel / arch / hostname

root_password = getpass.getpass("Enter root password: ")
ramblurr_password = getpass.getpass("Enter ramblurr password: ")

# Hash passwords using SHA-512
root_password_hash = sha512_crypt.hash(root_password)
ramblurr_password_hash = sha512_crypt.hash(ramblurr_password)

# Use ssh-keygen to generate ED25519 keys
with tempfile.TemporaryDirectory() as temp_dir:
    private_key_file = f"{temp_dir}/temp_ssh_key"
    public_key_file = f"{private_key_file}.pub"
    subprocess.run(
        [
            "ssh-keygen",
            "-C",
            hostname,
            "-t",
            "ed25519",
            "-N",
            "",
            "-f",
            private_key_file,
        ],
        check=True,
    )

    with open(private_key_file, "r") as file:
        private_key = file.read()

    with open(public_key_file, "r") as file:
        public_key = file.read().strip()

    age_key_pub = (
        subprocess.check_output(["ssh-to-age", "-i", public_key_file])
        .decode("utf-8")
        .strip()
    )


# Generate a machine-id
random_uuid = uuid.uuid4()
machine_id = random_uuid.hex
machine_id = machine_id.lower()

yaml_data = {
    "root-password": root_password_hash,
    "ramblurr-password": ramblurr_password_hash,
    "ssh_host_ed25519_key": LS(private_key),
    "ssh_host_ed25519_key_pub": public_key.strip(),
    "age_key_pub": age_key_pub,
    "machine_id": machine_id,
}

if host_path.exists():
    print(f"The host config path {host_path} already exists.")
    print("Aborting")
    sys.exit(1)
else:
    os.mkdir(host_path)


secrets_path = host_path / "secrets.sops.yaml"
with open(secrets_path, "w") as f:
    yaml = YAML()
    yaml.default_flow_style = False
    yaml.width = sys.maxsize
    yaml.dump(yaml_data, f)


print(f"Wrote {secrets_path}")

default_tmpl = """
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "%hostname%";
  machine-id = "%machineid%";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ./hardware.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = machine-id;
}
"""
default_path = host_path / "default.nix"
with open(default_path, "w") as f:
    c = default_tmpl.replace("%hostname%", hostname)
    c = c.replace("%machineid%", machine_id)
    f.write(c)

print(f"Wrote {default_path}")
