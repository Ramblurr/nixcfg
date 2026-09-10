# thinkpad1

**Buildable, not installable yet.** `storage.nix` contains build-only filesystem
placeholders. Replace them with the agreed disk configuration before installation
or deployment.

## Repository layout

This public directory contains the host configuration, hardware modules, storage
configuration, standalone installer module, and SOPS-encrypted host secrets.
Policy checks live in `tests/thinkpad1.nix` in this repository.

The private wrapper supplies `node.secretsDir` and `repo.secretFiles.*`.
`~/nixcfg-private/hosts/thinkpad1/secrets/local.nix` holds the real display name and
machine identity. Raw hardware evidence stays in the private repository too.
Never copy that data or plaintext secrets into this repository.

## Desktop and accounts

- Stable NixOS 26.05 with Plasma, German QWERTZ, `de_AT.UTF-8`, Europe/Vienna.
- Primary user `viki`: UID 1000, Bash, NetworkManager group, user-scoped Home Manager.
- Administrator `ramblurr`: UID 1001, wheel, repository-controlled SSH keys.
- Plasma Login Manager shows both normal accounts and preselects `viki`.
- Catppuccin Mocha/Mauve KDE defaults and Mocha Plymouth theme.
- NetworkManager, Bluetooth, PipeWire, firmware updates, power profiles, KDE Connect.
- Firefox and 1Password installed through Nix; no configured 1Password autostart.
- User-scoped Flathub for ordinary optional applications through Discover.
- No automatic NixOS or Flatpak updates; no Krohnkite.
- Plasma settings remain user-owned, including panels, wallpapers and shortcuts.
- SSH accepts keys only, permits root and the administrator, and is reachable only
  through the trusted Tailscale interface. Enroll with `sudo tailscale up` locally.

## Secret bootstrap (human-operated)

The committed `secrets.sops.yaml` is encrypted. Its host-key decryption has been
verified. Reuse the matching host key; generating another key requires updating
this host's recipient in the public `.sops.yaml` before encrypting again.

For a fresh bootstrap, run the generator outside both repositories:

```sh
umask 077
bootstrap=$(mktemp -d "$XDG_RUNTIME_DIR/thinkpad1-bootstrap.XXXXXX")
cd "$bootstrap"
mkdir hosts
nix develop ~/nixcfg --command python3 ~/nixcfg/scripts/gen-host.py thinkpad1
```

The generator prompts for root and administrator passwords and generates SSH/age
identity material. Do not install its generated `default.nix`: its filesystem
assumptions do not apply to this host.

Generate a password hash interactively:

```sh
nix shell nixpkgs#mkpasswd --command mkpasswd -m sha-512
```

Add the full hash as the quoted `viki-password` value in the generated YAML using
a private editor. Do not pass plaintext passwords as command arguments or paste
passwords, hashes, or private keys into agent conversations.

Update the host recipient in `~/nixcfg/.sops.yaml` to the generated `age_key_pub`,
retaining the administrator/recovery recipients. Encrypt to a temporary output
before replacing the committed ciphertext:

```sh
sops --encrypt \
  --config "$HOME/nixcfg/.sops.yaml" \
  --filename-override "$HOME/nixcfg/hosts/thinkpad1/secrets.sops.yaml" \
  --output "$bootstrap/secrets.encrypted.yaml" \
  "$bootstrap/hosts/thinkpad1/secrets.sops.yaml"
```

Verify decryption with the matching host key and confirm the values match the
input. Only then replace `~/nixcfg/hosts/thinkpad1/secrets.sops.yaml` with the
ciphertext. Preserve the old key/ciphertext until replacement recovery is verified.

Before first activation, securely install the matching private SSH host key at
`/mnt/etc/ssh/ssh_host_ed25519_key`, root-owned with mode 0600. Sops-nix uses it to
decrypt passwords on first boot. Keep a secure recovery copy before removing the
temporary plaintext material; runtime temporary directories do not survive logout
or reboot.

## Installation preparation

Boot the installer in UEFI mode and collect hardware/filesystem details privately.
Do not format a disk until its identity and the storage layout have been agreed.
Once the intended filesystems are mounted, obtain configuration without overwriting
files:

```sh
sudo nixos-generate-config --root /mnt --show-hardware-config
```

Use the actual filesystem, swap, and LUKS settings to replace `storage.nix`.
Keep the original Windows SSD untouched until the migration is verified.

## Build and validation

Commit public changes first with no unstaged tracked changes. From the private
wrapper:

```sh
cd ~/nixcfg-private
nix flake update
nix eval --impure --json .#nixosConfigurations \
  --apply "import $HOME/nixcfg/tests/thinkpad1.nix"
build thinkpad1
```

The standalone minimal installer contains no target identity, passwords, private
host key, or target disk layout. Build it from the private wrapper:

```sh
nix build .#thinkpad1-installer --out-link result-thinkpad1-installer
sha256sum result-thinkpad1-installer/iso/*.iso
```

The live image uses a German console keymap. Connect Wi-Fi with `nmtui`; SSH needs
an explicitly installed authorized public key. It does not partition disks.

On the installed laptop, verify login and user preselection, networking, audio,
Bluetooth, touch/pen, suspend/resume, and greeter readability. Test Discover with a
user-scoped Flatpak and confirm there are no automatic update timers. Change a
wallpaper or panel, rebuild, and confirm those personal settings survive.
