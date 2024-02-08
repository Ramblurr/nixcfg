import uuid
import subprocess
import tempfile
import sys
import os
from passlib.hash import sha512_crypt
import getpass
from ruamel.yaml import YAML
from ruamel.yaml.scalarstring import LiteralScalarString
import textwrap


def LS(s):
    return LiteralScalarString(textwrap.dedent(s))


hostname = input("Enter HOSTNAME: ")

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

    age_key_pub = subprocess.check_output(["ssh-to-age", "-i", public_key_file]).decode('utf-8').strip()


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
    "machine_id": machine_id
}

with open(f"{hostname}.yml", "w") as f:
    yaml = YAML()
    yaml.default_flow_style = False
    yaml.width = sys.maxsize
    yaml.dump(yaml_data, f)


print(f"Wrote {hostname}.yml")
