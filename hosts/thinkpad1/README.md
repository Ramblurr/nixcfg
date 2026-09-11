# thinkpad1

The laptop is installed with Btrfs and persistent swap inside LUKS2.
TPM deployment automation is under development. VM tests do not replace
the required hardware rollout and boot checks.

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
Plymouth uses its own `plymouthVariant` setting, currently Mocha for a legible
passphrase prompt. KDE stays Latte. Encrypted Btrfs device mounts wait without a
90-second deadline so a slow recovery-passphrase entry does not trigger emergency mode.

## TPM login PIN

Pinpam accepts a six-digit PIN for `viki` through the login PAM stack (used by
Plasma Login Manager and console login) and the KDE screen locker. Enter the PIN
in the existing password field. Password authentication remains available; SSH,
sudo, polkit, and other users do not gain PIN authentication.

From an administrator terminal, enroll with `sudo pinutil setup viki`.
Five failed PIN attempts lock the PIN until an administrator deletes and recreates
it. Do not clear the TPM: that also invalidates LUKS TPM enrollment. Normal users
must not join the `tss` group. The privileged pinutil wrapper mediates TPM access.

The experimental master-key/keyring feature is deliberately disabled. A PIN does
not itself unlock an existing password-protected KWallet or 1Password vault.

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

Syncthing runs as `viki`, with its GUI at `http://127.0.0.1:8384`. Its restored
device key and certificate come from the `syncthing-key` and `syncthing-cert` SOPS
secrets. Devices and folders are managed declaratively, as on quine; edits to
those lists in the web UI are replaced on service initialization.

Private `hosts/thinkpad1/secrets/syncthing.nix` holds the original device IDs,
labels, sharing assignments and folder settings, imported by `local.nix`. Public
`syncthing.nix` maps each folder to `/home/viki/Sync/<label>`. Imported folders
start paused until their data and ignore files have been checked; their `paused`
values are managed in the private Nix data.

Migration preserves the old configuration and database before changing identity,
and seeds the restored XML once to retain settings outside the declarative peer
and folder lists. The new identity must use a fresh database, not the previous
temporary identity's database. Do not run the old Windows device concurrently.
Syncthing is disabled in the disposable VM variant.

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

Secure Boot stays disabled to support standard-kernel hibernation. Disk unlock
uses SHA256 PCR4 and PCR9, with recovery-passphrase fallback. The login PIN is
separate from disk unlock. Never clear the TPM to repair either mechanism.

Supply `/run/thinkpad1-luks-passphrase` through nixos-anywhere's
`--disk-encryption-keys` option during installation only. Save the passphrase securely
outside the laptop and test it before enabling TPM unlock. Never place it in Nix
source or the Nix store. The TPM must unlock LUKS before LVM activates the resume
device. Verify cold boot, suspend, hibernate, resume, and snapshot file recovery
on the installed hardware; a successful build does not establish these behaviors.

## TPM-aware deployment

`deploy thinkpad1` uses the candidate system's `thinkpad1-tpm-deploy` helper.
Run it from a fresh private `nix develop` shell after updating the public input.
An older installed deploy wrapper does not provide this protection. Direct
`switch-to-configuration` and `nixos-rebuild` calls are not supported for updates.
For `switch` and `boot`, the helper prepares enrollment before boot installation.
It checks the selected boot entry and installed files before it reports success.
A working current disk enrollment authorizes this operation. The helper does not
read or store the plaintext volume key, and does not request the recovery password.
Candidates without the helper are rejected rather than deployed without this check.
Automatic enrollment also rejects active PIN-protected disk tokens. It does not
try PIN values. This restriction does not apply to the separate pinpam login PIN.

The helper supports settings, kernel, initrd, and unsigned systemd-boot updates.
Nix-prepended microcode is measured as part of the single combined initrd; separate
extra initrd images remain unsupported. A settings change still needs a new policy
because `init=` changes. Firmware overrides and unsupported boot layouts are rejected.
There is no automatic reboot. `test` and `dry-activate` do not enroll or change
the boot profile. Laptop rollout still requires approved physical validation.
The live firmware entry must be active and use the standard GPT HD/File/End path
to the observed systemd-boot image. Its partition UUID and geometry must match
the FAT partition mounted at `/boot`. Other paths, optional load arguments,
stacked mounts, and mounts of a partition subdirectory are rejected.
Persistent EFI preferences and `preferred` configuration directives are unsupported.
The helper accepts only the supported loader settings and Linux BLS entry fields.
It requires one default, one options line, one kernel, and one initrd. Extra entry
types and conflicting directives are rejected, including changes made by install hooks.
Kernel parameters must be non-empty printable strings without outer whitespace;
an empty parameter list is not supported. This keeps BLS parsing and PCR prediction
consistent. Nix can accept quoted line breaks that a BLS entry cannot preserve.
A boot log containing returned/retried EFI applications is unsupported: copying
those attempts into a next-boot prediction would be unsafe after a boot-order change.

A shared bootloader update also changes the measurement for rollback entries.
Before replacing either EFI copy, the helper prepares the candidate and retained
rollback systems, then temporarily selects the booted system as a bridge. That
bridge is covered under both the installed and desired loaders. Native `bootctl`
installs the exact desired version without changing firmware variables, including
on downgrade. The candidate becomes the default only after this preparation.
All boot installations use `/boot/loader/entries/thinkpad1-tpm-rollback.conf`
as this bridge. Its name keeps it outside native generation pruning. It remains
available as a rollback entry, with its system and policy retained. Do not edit it.
The helper flushes the EFI filesystem before retiring transition-only policies.

The journal is `/var/lib/thinkpad1-tpm-deploy/state.json`. It records enrollment
intent and fingerprints of owned tokens and slots. Recovery slot 0 and unmanaged
credentials are never retired. After a successful installation, the helper retains
the two most recent confirmed boot policies, their equivalents under the desired
loader, and the pending candidate: at most five automation-owned policies, plus
unmanaged credentials. Preparation can temporarily require more space. Confirmation
requires matching current boot measurements and successful TPM authorization;
activation alone is not proof.
Native profiles named `thinkpad1-tpm-<policy>` under
`/nix/var/nix/profiles/system-profiles/` keep the required systems available to the
boot loader and protect them from garbage collection. Separate profiles named
`thinkpad1-tpm-loader-<policy>` under `/nix/var/nix/profiles/` retain each policy's
loader package, which can differ from its system's package after a manual rollback.
Do not edit either set of managed profiles.

Failure handling:

- Before enrollment, capacity checks cover the whole missing policy set: available
  keyslot and token numbers, JSON metadata space, and contiguous keyslot storage.
  Insufficient capacity stops deployment without deleting old credentials to make
  room for an enrollment that has not yet succeeded.
- After an interruption, retry can adopt a unique matching new token. A new slot
  without a matching token requires manual review. The journal and slot are kept.
- After a boot installation error, the helper attempts to restore the bridge, both
  previous EFI images, and the previous profile/default. Prepared enrollments remain
  available for retry. This is not an atomic transaction across LUKS metadata and
  the EFI filesystem, nor a guarantee against filesystem damage during power loss.
- Do not reboot after a failed deployment until the boot default and recovery
  path have been checked. Do not delete the journal or wipe all TPM slots as a fix.

The cache under `/run/thinkpad1-tpm-deploy/` is valid only for the same boot, PCR
values, and measurement tool. Only immutable store-file digests are cached.
Installed EFI files are read again before and after installation. Current TPM
authorization is never cached. Timing output separates validation, PCR reads,
authorization, enrollment, and the native boot installation command. These are
command or phase times, not measurements of TPM hardware latency alone.

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
nix flake update nixcfg
nix develop
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
