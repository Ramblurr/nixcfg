# How to persist an existing Userborn account database

Migrate an already-Userborn host from `/etc` to `/var/lib/userborn`, backed by
`/persist/var/lib/userborn`. Keep its existing account IDs, retired-account
reservations, passwords, and rootless-container mappings.

## Before deploying

**Do not deploy this configuration before seeding its persistent database.**
Seed the source directory before activation. Systemd can create a missing bind
source, so a mounted directory alone is not proof of a valid migration. Userborn
refuses to start with missing/empty passwd, group, or shadow files; native
subordinate-ID support additionally requires both subordinate files to exist.

This procedure is for ordinary, non-overlay `/etc` and `users.mutableUsers = false`.
It is not an automatic Perl-to-Userborn migration or fresh-install bootstrap.
Do not initialize empty databases on an existing host. For a new installation,
provision the intended initial account database before its first boot.

Use a host-specific operation ledger and obtain approval for migration, activation,
and any reboot. Record exact old/new system paths and recovery access. Keep another
root session or console available. Freeze concurrent account/configuration changes.
Build and check the new configuration before the maintenance window.

The shared module changes storage on impermanent Userborn hosts. At introduction,
these are **Addams, Dewey, James, Mali, Peirce, Quine, Witt, and Debord**. Migrate each
before deploying it; **Debord goes last**. Non-impermanent hosts keep `/etc`.
Stable Userborn 0.5 retains the generated subordinate files; Userborn 1.0 uses native
ranges and bind mounts. Never change those mappings just to make migration pass.

## Inspect the target

Run on the target as root (local commands on Quine; normal SSH aliases elsewhere).
Put literal host/path values into the operation ledger before executing commands.

```bash
hostname
readlink -e /run/current-system
readlink -e /nix/var/nix/profiles/system
readlink -e /run/booted-system
systemctl show userborn.service -p ExecStart
findmnt --mountpoint /persist
findmnt -rn -o TARGET,FSROOT,OPTIONS | grep -E '^/etc/(passwd|group|shadow|subuid|subgid) '
stat -c '%n %u:%g %a %F' /etc/{passwd,group,shadow,subuid,subgid} /var/lib/userborn
find /var/lib/userborn -mindepth 1 -maxdepth 1 -printf '%f\n'
```

Require current system and profile to agree, Userborn's current location to be
`/etc`, and the five account paths to be regular files, not symlinks. Confirm the
read-only self-bind mounts belong to those exact `/etc` files. Stop on unexpected
mounts or Userborn-state contents. Inspect shadow **metadata only**.

Compare effective `/etc/subuid` and `/etc/subgid` ranges with the evaluated new
configuration. The existing formula is `100000 + UID * 65536`, with count `65536`;
`invoiceninja2` instead keeps start `362144`. Resolve any mismatch before proceeding.
Also record failed units, service health, the persistent filesystem's identity,
free space, and a recoverable storage snapshot where available.

## Back up and seed without changing identities

The following runs in a root Bash shell on the target. Replace `HOST` and
`OPERATION_ID` with the ledger's literal values. Backups stay on the target;
never print, upload, or put shadow contents into Nix expressions or Git.

```bash
set -euo pipefail
expected_host=HOST
operation_id=OPERATION_ID
[[ $(hostname) == "$expected_host" ]]
[[ $operation_id =~ ^[0-9]{8}T[0-9]{6}Z$ ]]
umask 077
backup="/persist/userborn-migration-$operation_id"
state=/persist/var/lib/userborn
stage="/persist/var/lib/.userborn-$operation_id"
for path in "$backup" "$state" "$stage"; do
  test ! -e "$path" && test ! -L "$path"
done
! mountpoint -q /var/lib/userborn
for file in passwd group shadow subuid subgid; do
  test -f "/etc/$file" && test ! -L "/etc/$file"
done
mkdir -m 0700 "$backup"
readlink -e /run/current-system > "$backup/system-current"
readlink -e /nix/var/nix/profiles/system > "$backup/system-profile"
cp -a /var/lib/userborn "$backup/userborn-state"
cp --preserve=all /etc/.clean "$backup/etc-clean"
awk -F: '{print $1 ":" $3 ":" $4}' /etc/passwd | sort > "$backup/user-ids"
awk -F: '{print $1 ":" $3}' /etc/group | sort > "$backup/group-ids"
install -d -m 0755 -o 0 -g 0 "$stage"
cp -a /var/lib/userborn/. "$stage/"
for file in passwd group shadow subuid subgid; do
  cp -L --preserve=all "/etc/$file" "$backup/$file"
  cp --preserve=all "$backup/$file" "$stage/$file"
  cmp -s "/etc/$file" "$stage/$file"
  test "$(stat -c '%u:%g:%a' "/etc/$file")" = "$(stat -c '%u:%g:%a' "$stage/$file")"
done
test "$(stat -c '%u:%g:%a' "$stage")" = 0:0:755
mv --no-clobber -T "$stage" "$state"
test ! -e "$stage"
sync
```

Keep the database directory traversable (`0755`) for NSS lookups. The shadow file
retains its restrictive mode; the backup directory remains `0700`. Do not recursively
chown application data or discard disabled account entries.

## Establish the bind, then activate

Still in the reviewed maintenance window, on the target:

```bash
mount --bind "$state" /var/lib/userborn
test "$state/passwd" -ef /var/lib/userborn/passwd
for file in passwd group shadow subuid subgid; do
  if mountpoint -q "/etc/$file"; then
    umount "/etc/$file"
  fi
  cmp -s "/etc/$file" "$state/$file"
done
```

The preceding inspection must have established that these are the old regular-file
self-binds. Do not reuse this unmount loop after migration, when passwd/group/shadow
are symlinks. The new Userborn unit only unmounts its **new** location and cannot
remove the old `/etc` mounts for the initial activation.

From `~/nixcfg-private` on the workstation, after target-specific approval:

```bash
swamp model method run host-HOST deploy --json
```

Use the actual definition name. Save the model's deployment record and verify its
system/profile identities. Do not continue after partial activation: inspect the
actual files, links, mounts, and units before choosing recovery.

## Verify before and after the separately approved reboot

On the target, require all pre-existing numeric IDs and effective subordinate
mappings to match the recorded baseline. Unexpected additions/removals also need
explanation; do not accept a successful service start alone.

```bash
awk -F: '{print $1 ":" $3 ":" $4}' /etc/passwd | sort > "$backup/user-ids-after"
awk -F: '{print $1 ":" $3}' /etc/group | sort > "$backup/group-ids-after"
diff -u "$backup/user-ids" "$backup/user-ids-after"
diff -u "$backup/group-ids" "$backup/group-ids-after"
diff -u <(sort "$backup/subuid") <(sort /etc/subuid)
diff -u <(sort "$backup/subgid") <(sort /etc/subgid)
findmnt --mountpoint /var/lib/userborn
test /etc/passwd -ef "$state/passwd"
test /etc/group -ef "$state/group"
test /etc/shadow -ef "$state/shadow"
stat -c '%n %u:%g %a' "$state" "$state/shadow"
systemctl is-active userborn.service
systemctl --failed --no-pager
```

For Userborn 1.0, also require `/etc/subuid` and `/etc/subgid` to be regular bind
mounts of the persistent files, not symlinks. Test non-root NSS lookups, inability
to read shadow, and `runuser -u USER -- unshare --user --map-auto -- true` for a
representative rootless account. Check SSH/sudo and the host's application health.
After a separately approved reboot, repeat these checks and verify the account
bind mounted in initrd before Userborn. Retain backups; do not remove the service
UID/GID pins as part of this migration.

## Roll back without losing the current database

A generation rollback alone is unsafe. The previous generation must see ordinary
account files **before** its Userborn runs. Keep any newer account/password state;
do not blindly restore the pre-migration shadow file or roll back application data.

With a root session/console and an approved recovery decision, copy the currently
authoritative five account files to a new root-only recovery directory. If the
current files are missing or inconsistent, stop and choose a reviewed backup instead.
Then, on the target (use the already recorded `backup` variable):

```bash
recovery="$backup/rollback-current"
mkdir -m 0700 "$recovery"
for file in passwd group shadow subuid subgid; do
  cp -L --preserve=all "/etc/$file" "$recovery/$file"
done
cp --preserve=all /etc/.clean "$recovery/etc-clean"
for file in subuid subgid; do
  test ! -L "/etc/$file"
  if mountpoint -q "/etc/$file"; then umount "/etc/$file"; fi
done
for file in passwd group shadow subuid subgid; do
  cp --remove-destination --preserve=all "$recovery/$file" "/etc/$file"
done
awk '$0 != "passwd" && $0 != "group" && $0 != "shadow" && $0 != "subuid" && $0 != "subgid"' \
  /etc/.clean > "$recovery/clean-filtered"
cp "$recovery/clean-filtered" /etc/.clean
```

Removing only those exact names from `.clean` is essential: `setup-etc` otherwise
unconditionally deletes them as obsolete managed files, even after they have been
replaced with ordinary copies. Preserve every unrelated entry.

Restore the exact previous system profile and activate its system using the
model-recorded rollback commands. Recheck IDs, subordinate mappings, authentication,
and applications. Keep persistent state and backups for investigation. The old
configuration still lacks account persistence: **do not reboot it casually** or
reuse stale seeded files for another forward migration.

## Related evidence

- [Userborn issue 35: lost account files on impermanent roots](https://github.com/nikstur/userborn/issues/35)
- [Native subordinate-ID support](https://github.com/nikstur/userborn/pull/48)
- [NixOS subordinate-file bind mounts](https://github.com/NixOS/nixpkgs/pull/508608)
- [Pinned setup-etc bookkeeping](https://github.com/NixOS/nixpkgs/blob/79f0161625eb724a34cc3b527ba14ce19617dbe1/nixos/modules/system/etc/setup-etc.pl)
