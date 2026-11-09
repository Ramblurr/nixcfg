# Maintain dependency mirrors

Use `https://src.<work-domain>/edit/` for the editor and
`https://src.<work-domain>/` for cgit. The hostname comes from Peirce's configured
work domain. LAN and Tailscale DNS point directly to Peirce; there is no public DNS
record or public ingress route. Caddy serves HTTPS, with both backends bound to
loopback. There is no app login. The `/edit` namespace is reserved for the editor.
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

## Agent HTTP API

All paths below are relative to `/edit`. Access uses the same private ingress as
the editor; there is no separate API credential. Do not expose this service publicly.

- `GET /api/repos`: JSON object keyed by canonical URL, with options as values.
  Read the `X-CSRF-Token` response header and send it on every mutation.
- `PATCH /api/repos`: `Content-Type: application/merge-patch+json`.
  JSON Merge Patch: `{ "https://github.com/example/project": {} }` inserts an
  entry or leaves an existing one unchanged. Option fields merge; `null` removes
  an option. Arrays such as `include` replace the whole array. A URL mapped to
  `null` removes that inventory entry, retaining mirrored files.
- `DELETE /api/repos`: `Content-Type: application/json`, body
  `{ "url": "https://github.com/example/project" }`. Same removal semantics;
  deleting an absent URL is a no-op.
- `POST /api/sync`: `Content-Type: application/json`, body
  `{ "url": "https://github.com/example/project" }` for one existing entry or
  `{ "all": true }` for all entries. Returns `202` on acceptance, not completion.

Options use the EDN field names as JSON strings, including question marks:
`"issues?"`, `"wiki?"`, `"exclude-forks?"`, `"contributed?"`, `"gists?"`,
`"organization?"`, `"include"`, `"provider"`, `"scope"`. Provider/scope values are
strings without a colon. Do not include `url` inside an options object.
Use canonical URLs from GET: no trailing slash or forge `.git` suffix; GitHub
paths are lowercase. Generic Git URLs can retain `.git`.

Patches validate the complete resulting inventory and write atomically under the
same inventory lock as the UI. One invalid entry rejects the whole batch; omitted
entries/options survive. Requests are limited to 16 KiB. Split larger batches;
atomicity applies per request. Writes do not trigger syncs. Concurrent edits to
the same field use the last committed value; this API has no revision precondition.

Command-line clients may omit `Origin`. If supplied, it must match the configured
public origin. Cross-site requests and missing/stale CSRF tokens return `403`;
refetch the token after an editor restart. No CORS access is granted. Wrong media
types return `415`, validation failures `400`, and an already-running editor sync
`409`. Scheduled syncs share the filesystem lock; check the journal for completion
or lock contention after `202`, just as for UI syncs.

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

For GitHub credentials, set `modules.services.dependency-mirror.githubTokenReference`
to an `op://...` reference. The systemd credential provider delivers it to both
services; `GITHUB_TOKEN_FILE` points at each unit's private runtime credential.
Peirce uses `op://home-ops-prod/gickup/ol-git-archive`. Restart the editor after
rotating the token; scheduled syncs load it at each start.

Other providers use `<PROVIDER>_TOKEN_FILE` on both services, pointing at a runtime
secret readable by `dependency-mirror`. Never put tokens in URLs, EDN, or Nix
literals. Providers may require API credentials even for public repositories.

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
git ls-remote https://src.<work-domain>/github.com/cooperspencer/gickup.git
git clone https://src.<work-domain>/github.com/cooperspencer/gickup.git
```

The cgit About tab reads the first matching README from the selected/default
branch. Markdown, AsciiDoc (`.adoc` and `.asciidoc`), and plain text are supported.
The packaged filter uses Python Markdown and Asciidoctor, sanitizes their HTML,
and disables AsciiDoc includes. Rendering is limited to one million characters;
use the tree view for larger files. It never checks out repository content or
runs repository-provided code.

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

This starts only the editor, not cgit. To test a proxy mount, add
`--base-path /edit --public-origin http://127.0.0.1:8084`; forms and assets retain
the prefix. The production origin is explicit: forwarded headers cannot change
the accepted Host or Origin. The Nix-packaged `dependency-mirror` command
bundles runtime dependencies and browser assets for offline startup.
Run `dependency-mirror --test` for the embedded tests, including a local Git
clone/update fixture when Gickup is available.
