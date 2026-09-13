# Databasus backups

Databasus runs on Mali and backs up PostgreSQL workloads. This guide covers the
physical backup chains for Dewey's host PostgreSQL and its `pg-matrix` container.

## Deployment and access

- Service configuration: [Databasus module](../../modules/services/databasus/default.nix).
- Host assignment and domain: [Mali configuration](../../hosts/mali/default.nix),
  `modules.services.databasus.domain`.
- Access: HTTPS through Caddy, restricted to configured peer addresses, followed
  by Databasus login.
- Unit: `podman-databasus.service` on Mali; web port is loopback-only, default 4005.
- API contract and operational limitations below were verified with v3.53.0.

Backup setup, recovery, restores and service changes are live operations. Obtain
human approval and use the Operational Change Protocol before making changes.
This document is not standing authorization to execute its mutation example.

## Storage and application database

`/var/lib/databasus` is mounted at `/databasus-data` inside the container. The
module creates an encrypted ZFS dataset for this directory. It holds application
state, the encryption key and local backups. Databasus uses its embedded
PostgreSQL instance for application state.

The container health check requires both `pg_isready` against internal
PostgreSQL on port 5437 and `databasus healthcheck`. A responding web page alone
is not sufficient evidence that the internal database works.

Keep the module's data-directory settings: UID 65532, GID 999,
mode 0750 for the pinned image. Do not reset it to `root:root`: that caused an
internal PostgreSQL permission failure during a previous activation.

The two documented physical registrations use encrypted local storage and
five-chain retention. Encryption does not establish that an off-host copy exists.
Verify current storage and retention in Databasus before changes.
Changing an existing storage assignment can delete backups; it is not a routine
way to repair a broken chain. Preserve existing backups and the encryption key.

## Source clusters and backup roles

| Databasus name | Source | Connection through Dewey | Replication role |
| --- | --- | --- | --- |
| `pg-dewey` | Host PostgreSQL | Management address, port 5432 | `databasus_pg_dewey` |
| `pg-matrix` | PostgreSQL in the `pg-matrix` container | Management address, proxy port 5433 | `databasus_pg_matrix` |

Source wiring and role provisioning are in
[postgresql.nix](../../modules/services/postgresql.nix) and
[matrix-synapse-postgres.nix](../../modules/services/matrix-synapse-postgres.nix).
Passwords are delivered at runtime through the configured 1Password credential
consumers. Do not copy passwords into documentation or command arguments.

These are PostgreSQL 18 physical backups, not per-database logical dumps.
Both sources enable WAL summarization. At the September 2026 verification,
PostgreSQL retained WAL summaries for ten days. The logical-backup role
`pg_read_all_data` is not a substitute for the physical replication-role setup.

## CLI workflow

Use the typed `databasus-home` Swamp model from `~/nixcfg-private`. Its detailed
method reference is `models/@ramblurr/databasus/README.md` in that repository.
The older public `scripts/databasus` helper remains available and unchanged;
this guide uses the guarded model operations used during recovery.

Authentication comes from the `op-private` vault. Keep `op signin` and the
commands that need its session in the same shell. Do not print or persist session
values, passwords or JWTs.

Discover current IDs rather than assuming a display name identifies one entry:

```sh
cd ~/nixcfg-private
swamp model method run databasus-home health
swamp model method run databasus-home listWorkspaces
swamp model method run databasus-home listDatabases --input 'workspaceId=<UUID>'
swamp model method run databasus-home getBackupConfig --input 'databaseId=<UUID>'
swamp model method run databasus-home listBackups --input 'databaseId=<UUID>'
```

These model views intentionally omit credentials and free-form errors. They are
not configuration exports that can safely be submitted back to an update API.
Use `setDatabaseNotifiers` for guarded, credential-preserving assignments; see
the private model reference for the complete input and confirmation requirements.

## Schedule, alerts and verification

The recovered physical chains were configured for:

- `FULL_INCREMENTAL` mode;
- a Sunday full backup at 02:17 UTC;
- a daily incremental at 03:17 UTC;
- encrypted storage with five-chain retention.

Treat these as the recorded operating policy, not proof of current live settings.
Use `getBackupConfig` and backup history to verify the actual configuration.
Check completion time, status and root-full identity for each source. A trigger
being accepted does not prove a job started or completed; paginated history can
also omit a job outside the inspected range.

Both registrations have the existing `Databasus email` notifier attached. It
uses the local Maddy relay. Notifier assignments and the backup policy's event
settings are separate: inspect both, including `BACKUP_FAILED` and
`CHAIN_BROKEN`. The single test email was delivered and human receipt was
confirmed on September 13, 2026. Do not resend it without approval.

Gatus on Debord checks `/api/v1/system/health`, requires HTTP 200 and retains
Pushover alerts. **A green health check does not prove that backups are recent
or restorable.** Per-database backup-freshness monitoring is intentionally outside
this work's scope. Email transport testing does not prove every failure-event
notification path.

## Operational limits and broken-chain recovery

Two separate limitations were observed with Databasus v3.53.0:

1. **Recovery scheduling:** after an outage exceeds WAL-summary retention, an
   incremental can fail with `CHAIN_BROKEN` / `SUMMARIES_EXPIRED`. The scheduler
   can wait for the next weekly full-backup slot instead of immediately creating
   a replacement full. Service recovery does not itself end the backup gap.
2. **Failure detail:** the physical backup API omits its internal `ErrorReason`.
   The Swamp model deliberately excludes free-form `failMessage` and does not
   guess a structured reason. Missing detail is not success. Inspect bounded
   server logs to establish the cause, without copying secrets or raw logs into
   tickets or chat. This reporting limitation does not itself break backups.

We accept these limits operationally. No upstream report, API extension or local
scheduler patch is planned for this follow-up.

When a chain cannot continue:

1. Confirm the exact source, workspace, failed job and chain. Check service and
   source health, available storage and whether another backup is running.
2. Inspect bounded logs on Mali, for example
   `sudo journalctl -u podman-databasus.service --since '<UTC time>' --until '<UTC time>' --utc --no-pager`.
   Diagnose the cause; do not assume every failed incremental means expired WAL
   summaries. Use normal SSH only when Mali is not the operator's current host.
3. Obtain approval for a replacement full backup and record the plan. Do not
   delete old chains, change storage or retry incrementals blindly.
4. For a confirmed broken physical chain, use the guarded full-backup request:

   ```sh
   swamp model method run databasus-home startBackup \
     --input '{"databaseId":"<UUID>","workspaceId":"<UUID>","expectedName":"<exact-name>","confirm":true,"backupType":"full"}'
   ```

5. Verify the newly created full reaches completed status. Later, verify that a
   scheduled incremental completes against that new full root. Do not infer
   completion from the request response or simply select an older completed job.

The confirmation must be JSON boolean `true`, not the string passed by
`--input confirm=true`. The guard does not replace human operational approval.

## Restore notes and recovery evidence

Use an explicitly approved, isolated restore target. Never point a test restore
at a production database. Preserve the original backups and encryption key.
Use the supported Databasus physical export. A physical chain is not a logical
dump and cannot be restored with `pg_restore`.

A physical-chain test must verify export hashes, safely extract the bundle,
combine the full and incrementals in order with matching PostgreSQL tooling,
and run `pg_verifybackup` including WAL checks before isolated startup. Disable
restored hooks and event triggers; enforce resource and network isolation.
The supported export's single-use restore token can appear in backend URL logs.
The previous test had explicit human approval for that narrow exception; it is
not standing approval for future exports. Never copy tokens or encryption keys
into reports.

Both recovered chains passed isolated PostgreSQL 18.6 startup and exact source
metadata comparisons on September 12, 2026. All test files and units were then
removed. This establishes the tested chains' startup/catalog integrity, not all
application rows, arbitrary point-in-time recovery or every older backup.

Private operational evidence remains in `~/nixcfg-private`:

- `incidents/2026/09-10 Databasus physical backup chains broken.md` records the
  recovery, restore checks, cleanup, email confirmation and Gatus deployment.
- `models/@ramblurr/databasus/README.md` documents model methods and safety guards.

The incident links the local operation ledgers and the isolated restore plan.
Use them as evidence and planning references, not unattended restore scripts.

Guide structure adapted from `~/src/sno/infra/docs/002-ops-postgres-databasus-backups.md`;
deployment details and procedures above describe this repository's setup.
