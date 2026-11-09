# Maintain dependency mirrors

Use the editor at <http://100.64.18.17:8084/> on the tailnet. Browse and clone
mirrors through cgit at <http://100.64.18.17:8085/>. Both listeners bind only to
Peirce's Tailscale address; tailnet policy controls access. There is no app login.
This service is separate from personal GitHub backups and Forgejo.

## Select and update repositories

Enter an HTTPS repository URL or a forge user/organization URL. Known hosts are
detected automatically; select the provider for a self-hosted forge. Save extras
on existing rows before syncing. Exclude forks defaults to on; contributed
repositories and gists default to off. Options appear only for supporting adapters.

Use **Sync** for one entry or **Sync All** for the inventory. The daily timer runs
at 04:30 in the host's timezone, with up to 15 minutes of jitter and catch-up after
downtime. Changes made during a sync apply to the next run. Deleting an entry stops
future selection; it does not delete existing mirrors or cancel an active sync.

## Edit the inventory directly

The source of truth is `/var/lib/dependency-mirror/repos.edn`; changes need no Nix
deployment. Avoid editing while the UI is writing. Preserve ownership
`dependency-mirror:dependency-mirror` and mode `0640` or stricter.

For an owner with a selected subset:

```clojure
{:repos [{:url "https://github.com/cooperspencer"
          :include ["gickup"]
          :issues? true
          :exclude-forks? true}]}
```

For self-hosted or ambiguous paths, use `:provider` (such as `:gitea` or `:gitlab`)
and `:scope :owner` or `:scope :repo`. GitLab groups require
`:organization? true`; Gickup's group enumeration requires an authenticated token
owner. Nested GitLab group paths cannot be distinguished from repository paths
by URL alone. The UI handles ordinary owner/repository paths; use EDN for these
ambiguous selections.

For credentials, supply `<PROVIDER>_TOKEN_FILE` (for example `GITHUB_TOKEN_FILE`)
to **both** systemd services, pointing at a runtime secret readable by
`dependency-mirror`. Use the existing secret-delivery machinery. Never put tokens
in URLs, EDN, or Nix literals. Providers may require API credentials even for
public repositories. No credentials are provisioned by this module by default.

## Verify a sync

On Peirce:

```sh
sudo systemctl start dependency-mirror-sync.service
sudo journalctl -u dependency-mirror -u dependency-mirror-sync --since '-15 min'
systemctl list-timers dependency-mirror-sync.timer
```

The UI deliberately has no sync-history display. Check the journal and cgit;
Gickup can log individual repository failures without returning a failing exit
status. A successful unit alone is not proof that every repository updated.

Copy the clone URL from cgit. For the example above:

```sh
git ls-remote http://100.64.18.17:8085/github.com/cooperspencer/gickup.git
git clone http://100.64.18.17:8085/github.com/cooperspencer/gickup.git
```

HTTP access is read-only. LFS objects, release assets, and implicit submodules are
not downloaded. Issues and wikis follow Gickup's adapter behavior; issue JSON is
stored alongside the mirrors, not displayed as a cgit issue tracker.

## Storage and recovery

The inventory and repositories live on
`rpool/encrypted/safe/svc/dependency-mirror`, mounted at
`/var/lib/dependency-mirror`. Existing Peirce safe-dataset replication covers it.
Nix creates the dataset before tmpfiles and services run. Removing the module or
rolling back NixOS does not destroy the dataset.

Stop the editor and sync timer before restoring inventory from a snapshot. Do not
roll back the whole dataset merely to undo an inventory edit: that also rewinds
mirrors. Keep mirrors after removing entries unless their deletion is separately
approved.

## Run locally

With Babashka and Gickup on PATH, from the public checkout:

```sh
state=$(mktemp -d)
bb scripts/dependency-mirror.clj serve --state-dir "$state" --bind 127.0.0.1 --port 8084
```

This starts only the editor, not cgit. The Nix-packaged `dependency-mirror` command
bundles runtime dependencies and browser assets for offline startup.
Run `dependency-mirror --test` for the embedded tests, including a local Git
clone/update fixture when Gickup is available.
