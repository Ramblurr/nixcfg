# thinkpad1

**Disk layout prepared; installation not yet authorized.** `disk-config.nix` defines
Btrfs and persistent swap inside LUKS2. Hardware validation, TPM enrollment,
recovery-key verification, and backup completion remain installation gates.

## Repository layout

This public directory contains the host configuration, hardware modules, storage
configuration, and SOPS-encrypted host secrets.
Policy checks live in `tests/thinkpad1.nix` in this repository.

The private wrapper supplies `node.secretsDir` and `repo.secretFiles.*`.
`~/nixcfg-private/hosts/thinkpad1/secrets/local.nix` holds the real display name and
machine identity. Raw hardware evidence stays in the private repository too.
Never copy that data or plaintext secrets into this repository.

## Desktop and accounts

- Stable NixOS 26.05 with Plasma, US English locale, Austrian German QWERTZ, Europe/Vienna.
- Primary user `viki`: UID 1000, Bash, NetworkManager group, user-scoped Home Manager.
- Administrator `ramblurr`: UID 1001, wheel, repository-controlled SSH keys.
- Plasma Login Manager shows both normal accounts and preselects `viki`.
- Catppuccin KDE and Plymouth themes, with selectable variant and KDE accent.
- NetworkManager, Bluetooth, PipeWire, firmware updates, power profiles, KDE Connect.
- Firefox and 1Password installed through Nix; no configured 1Password autostart.
- User-scoped Flathub for ordinary optional applications through Discover.
- No automatic NixOS or Flatpak updates; no Krohnkite.
- Plasma settings remain user-owned, including panels, wallpapers and shortcuts.
- SSH accepts keys only, permits root and the administrator, and is reachable only
  through the trusted Tailscale interface. Enroll with `sudo tailscale up` locally.

## Theme variant

Change `catppuccinVariant` in `default.nix` to `latte`, `frappe`, `macchiato`, or
`mocha`. That single setting selects the KDE global theme, color scheme, and
Plymouth theme. Existing per-user theme choices take precedence over system defaults.

Set `catppuccinAccent` to `mauve`, `lavender`, `teal`, `sapphire`, or `sky` for KDE.
Plymouth follows the variant only.

## Backups and synchronization

`borgmatic.nix` prepares separate `thinkpad1` repositories on `mali` and offsite2.
It backs up `/home` and `/etc`, excluding caches, trash, and reinstallable Flatpak
packages. Flatpak application data and Syncthing configuration remain included.
The shared module supplies daily scheduling and the same retention policy as quine.

Borgmatic is deliberately disabled until repository provisioning is complete:

1. Generate a new dedicated, non-interactive Borg SSH key, distinct from the host
   SSH key and quine's backup key. Keep the private key out of Git.
2. Register its public key with a restricted `thinkpad1` repository on each server.
3. Use SOPS to add `borgmatic-ssh-key` and `borgmatic-env` to this host's encrypted
   YAML. The latter must be a multiline environment-file string containing
   `PASSPHRASE`, `NAS_REPOSITORY`, `OFFSITE_REPOSITORY2`, and
   `BORGMATIC_GATUS_TOKEN`. Use a new Borg encryption passphrase and new repository
   paths, not quine's existing repositories.
4. Verify server host keys, initialize the encrypted repositories, enable Borgmatic
   in `borgmatic.nix`, and test a backup and restore before relying on the timer.

Syncthing runs as `viki`. Open `http://127.0.0.1:8384` on the laptop and configure
devices and folders through its web UI; rebuilds preserve those choices. The GUI
is loopback-only, while the normal sync/discovery ports are open. A fresh identity
is generated in `~/.config/syncthing`; no default folder or peer is configured.
Do not reuse the disposable VM's Syncthing identity for the installed laptop.

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

## Filesystems and recovery

- GPT with a 1 GiB EFI filesystem at `/boot`; the remaining space is LUKS2.
- LVM inside LUKS contains 24 GiB of persistent swap and a Btrfs filesystem.
  Swap uses the same outer encryption as the filesystem, not a new random key
  each boot. The swap LV is the explicit hibernation resume device.
- Btrfs subvolumes separate `/`, `/home`, `/nix`, `/var/log`, and `/home/.snapshots`.
- Snapper takes hourly snapshots of both home directories. Retention is 24 hourly,
  7 daily, 4 weekly, and 3 monthly snapshots. These are count limits, not a hard
  disk-space quota. Monitor free space; changed or deleted files can retain space.
- Snapshot access is administrator-only. Inspect with `sudo snapper -c home list`.
  Restore selected files from `/home/.snapshots/<number>/snapshot/`, preserving
  ownership. Do not roll back the entire mounted home subvolume during a session.
- Borg excludes `/home/.snapshots`; it backs up the current home data separately.
  Local snapshots are not a substitute for backups. Application databases may
  require application-specific recovery because snapshots are not app-quiesced.

Secure Boot stays disabled to support standard-kernel hibernation. The initrd
supports TPM2 unlock with passphrase fallback, but no TPM policy is enrolled by
this configuration. Before enrollment, confirm TPM2 availability and select a
measured-boot policy that checks boot components, not merely disabled Secure Boot.
Boot changes may require the recovery passphrase and policy re-enrollment.

Disko asks for the LUKS recovery passphrase during formatting. Save it securely
outside the laptop and test it before enabling TPM unlock. Never place it in Nix
source or the Nix store. The TPM must unlock LUKS before LVM activates the resume
device. Verify cold boot, suspend, hibernate, resume, and snapshot file recovery
on the installed hardware; a successful build does not establish these behaviors.

## Installation preparation

Boot the installer in UEFI mode and collect hardware/filesystem details privately.
Do not format a disk until its identity and the storage layout have been agreed.
Once the intended filesystems are mounted, obtain configuration without overwriting
files:

```sh
sudo nixos-generate-config --root /mnt --show-hardware-config
```

Compare the resulting filesystem, swap, and LUKS settings with `disk-config.nix`.
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

Installation will use a standard NixOS live USB and nixos-anywhere from the private
wrapper. Disko supplies the agreed layout from `disk-config.nix`; the target SSD's
stable ID is held in private `local.nix` as `systemDisk`. Reconfirm its identity
and verify the Windows backup before authorizing formatting.

Enable key-based SSH access in the live environment. Supply the decrypted host
key through nixos-anywhere's `--extra-files` mechanism, at
`etc/ssh/ssh_host_ed25519_key` within the staging directory, with mode 0600.
Do not run Disko or nixos-anywhere until the destructive disk operation is approved.

On the installed laptop, verify login and user preselection, networking, audio,
Bluetooth, touch/pen, suspend/resume, and greeter readability. Test Discover with a
user-scoped Flatpak and confirm there are no automatic update timers. Change a
wallpaper or panel, rebuild, and confirm those personal settings survive.

## Desktop VM

The VM variant uses a virtual disk, a separate hostname, and password `test` for
`viki`, `ramblurr`, and root. SOPS secrets, SSH and Tailscale are disabled in the VM.
These overrides do not apply to the installed host.

Build the launcher from the private wrapper:

```sh
cd ~/nixcfg-private
nix build .#nixosConfigurations.thinkpad1.config.system.build.vm --out-link result-thinkpad1-vm
```

Run `result-thinkpad1-vm/bin/run-thinkpad1-vm-vm` from a disposable working directory.
It opens a QEMU window and creates its virtual disk in the working directory.
This tests the desktop, not installation, encryption, or physical hardware.
