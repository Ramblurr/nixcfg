#!/usr/bin/env python3
import argparse
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


def write_secrets(host_path: Path, secrets_data):
    secrets_path = host_path / "secrets.sops.yaml"
    with open(secrets_path, "w") as f:
        yaml = YAML()
        yaml.default_flow_style = False
        yaml.width = sys.maxsize
        yaml.dump(secrets_data, f)


def write_nix(host_path: Path, nix_data, interactive=False):
    default_path = host_path / "default.nix"
    if default_path.exists():
        if interactive:
            print()
            print(f"The {default_path}/default.nix already exists.")
            print()
            print(f"")
            options = ["[y] yes [n] no"]
            terminal_menu = TerminalMenu(
                options, title="Would you like to overwrite it?"
            )
            menu_entry_index = terminal_menu.show()
            if menu_entry_index != 0:
                print("Aborting")
                sys.exit(1)
        else:
            print(f"The default.nix file already exists at {default_path}")
            print("Not overwriting it")
            return
    with open(default_path, "w") as f:
        f.write(nix_data)
    print(f"Wrote {default_path}")


def ensure_host_path(host_path: Path, interactive=False):
    if host_path.exists():
        if interactive:
            print()
            print(f"The host config path {host_path} already exists.")
            print()
            print(f"")
            options = ["[y] yes [n] no"]
            terminal_menu = TerminalMenu(
                options, title="Would you like to overwrite the existing secrets file?"
            )
            menu_entry_index = terminal_menu.show()
            if menu_entry_index != 0:
                print("Aborting")
                sys.exit(1)
        else:
            print(f"The host config path {host_path} already exists, aborting.")
            sys.exit(1)
    else:
        os.mkdir(host_path)


def generate_machine_id():
    random_uuid = uuid.uuid4()
    machine_id = random_uuid.hex
    return machine_id.lower()


def generate_ssh_key(hostname):
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
        return private_key, public_key, age_key_pub


def hash_password(pw):
    return sha512_crypt.hash(pw)


def prompt_host_data():
    root_password = getpass.getpass("Enter root password: ")
    ramblurr_password = getpass.getpass("Enter ramblurr password: ")
    return hash_password(root_password), hash_password(ramblurr_password)


def prompt_guest_data():
    pass


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--type",
        choices=["host", "guest"],
        default="host",
        help="The type of nixos system to generate",
    )
    parser.add_argument(
        "--non-interactive",
        action="store_true",
        default=False,
        help="Run in non-interactive mode",
    )
    parser.add_argument("hostname", nargs="?", help="The hostname of the machine")

    args = parser.parse_args()
    hostname = args.hostname
    interactive = not args.non_interactive

    print()
    print(f"Generating {args.type} config for {hostname}")
    print()

    if args.type == "host":
        host_path = Path("./hosts") / hostname
        root_password_hash, ramblurr_password_hash = prompt_host_data()
        machine_id = generate_machine_id()
        private_key, public_key, age_key_pub = generate_ssh_key(hostname)
        secrets_data = {
            "root-password": root_password_hash,
            "ramblurr-password": ramblurr_password_hash,
            "ssh_host_ed25519_key": LS(private_key),
            "ssh_host_ed25519_key_pub": public_key.strip(),
            "age_key_pub": age_key_pub,
            "machine_id": machine_id,
        }
        nix_data = """
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
in
{
  imports = [
    ./hardware.nix
  ];
  system.stateVersion = "24.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";

  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    impermanence.enable = true;

    boot.zfs = {
      enable = true;
      encrypted = true;
      rootPool = "rpool";
      scrubPools = [ "rpool" ];
      extraPools = [ ];
      autoSnapshot.enable = false;
    };
    zfs.datasets.enable = true;
    security.default.enable = true;
    firewall.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
    ];
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [ ];
  };

  systemd.tmpfiles.rules = [
    "d /persist/home/${username} 700 ${username} ${username}"
    "d /persist/home/${username}/.config 0775 ${username} ${username}  -"
    "d /persist/home/${username}/.local 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state/zsh 755 ${username} ${username}"
  ];
}
"""
    elif args.type == "guest":
        host_path = Path("./guests") / hostname
        machine_id = generate_machine_id()
        private_key, public_key, age_key_pub = generate_ssh_key(hostname)
        secrets_data = {
            "ssh_host_ed25519_key": LS(private_key),
            "ssh_host_ed25519_key_pub": public_key.strip(),
            "age_key_pub": age_key_pub,
            "machine_id": machine_id,
        }
        nix_data = """
{
  lib,
  config,
  pkgs,
  ...
}:
{
  system.stateVersion = "24.11";
}
"""

    ensure_host_path(host_path, interactive)
    write_secrets(host_path, secrets_data)
    write_nix(host_path, nix_data, interactive)


if __name__ == "__main__":

    main()
