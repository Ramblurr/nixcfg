# ts-broker

ts-broker decides which 1Password credential an agent may have injected by
ts-egress. It runs next to ts-egress on the egress MicroVM. A human approves
requests in a web UI, so a grant does not need a Tailscale policy push.

- The policy caps set the ceiling: an identity's groups and allowed modes.
- The broker database holds the grants below it: credentials, assignments to
  groups or identities, and approvals.
- 1Password holds the values. The broker stores vault, item and field IDs
  and never reads a value.

## Running

```sh
ts-broker serve --db /var/lib/ts-broker/broker.db \
  --socket /run/ts-broker/decide.sock --port 795 \
  --connect-url http://op-connect:8080
```

The broker creates the schema in a new database and refuses to start on a
database whose `user_version` is not the current schema version. It does
not migrate.

The Connect token comes from `--connect-token-file` or the
`op-connect-token` systemd credential. `modules/services/ts-broker.nix` runs
the service. The socket file has mode 0660 and the service's group,
`ts-broker-clients`, so only the ts-egress service can connect. The UI
listens on 127.0.0.1 only, and the module's nftables rule drops loopback
connections to it from every user but root, which tailscaled runs as. The
UI port is privileged (795 by default), and `CAP_NET_BIND_SERVICE` is the
service's only capability, so no other user can listen there while the
broker is down. The service is bound to `nftables.service`: when the
firewall stops or fails, the broker stops.
Expose it with `tailscale serve`. Every request must carry the
`Tailscale-User-Login` header that serve sets, or the broker answers 403.
The login is recorded as the editor or approver.

Every response forbids framing (`Content-Security-Policy: frame-ancestors
'none'` and `X-Frame-Options: DENY`). A POST needs `Datastar-Request: true`
and `Sec-Fetch-Site: same-origin`, which the Datastar client and the
browser send. Actions read their parameters from the request body only.

Live re-renders skip the credential edit form, so a draft and the revision
it was opened at stay until a save or a reload. A save from a form whose
credential changed since is refused. A successful save replaces the form
with one at the new revision.

## Decide protocol

ts-egress writes one JSON request per line to the socket and reads one JSON
response line. The full contract is
`.scratch-org/109-egress-credential-broker/contract.org`.

```json
{"op": "decide", "identity": "tag:t3-alice", "node": "t3-alice", "groups": ["agents"],
 "modes": ["inject"], "credential": "GITHUB_TOKEN", "host": "api.github.com",
 "header": "Authorization", "mode": "inject"}
```

`header` is the header the placeholder was found in.

| Response | When |
|----------|------|
| `{"decision": "allow", "approval": id or null, "secret": {vault, item, field}, "inject": {header}}` | An `auto` assignment matches, or a live approval exists. The allow counts as one use. |
| `{"decision": "pending", "approval": id}` | An `ask` assignment matches and no approval is live. The broker opens one pending approval and emits one `approval-needed` event. |
| `{"decision": "deny", "reason": r}` | `malformed`, `mode` (not in the request's modes), `unknown`, `host`, `header` (not the credential's header, compared without case), `unassigned`, `blocked` |

The checks run in that order, so a request in the wrong header neither
opens nor spends an approval. Assignments match `group:<g>` for each of the
request's groups, or the exact identity. If the store fails, the broker
closes the connection, and ts-egress fails closed.

A request is `malformed` when its line is not exactly one JSON object,
misses a field, names a credential that is not 1 to 64 of `A-Z`, `0-9` and
`_` (not starting with a digit), has a `header` that is not a header name,
or passes a length limit. A malformed request adds no audit row, only one
line on stderr. A request line must end in a newline, arrive within 5
seconds and be at most 64 KiB. A longer line, or a last line without its
newline, also closes the connection. Beyond 32
open connections the broker closes new ones at once.

Approvals follow this state machine (`ts-broker.domain/transitions`):

```text
pending  -approve(once | 1h | 24h | forever)-> approved
pending  -deny->    denied    the next request opens a new pending approval
pending  -block->   blocked   requests are denied until unblocked
approved -revoke->  revoked
approved -expire->  expired   the expiry passes or the once use is spent
blocked  -unblock-> closed
pending, approved -supersede-> superseded   the credential is edited or deleted
```

An approval records the credential revision and the host it was requested
under. Saving a changed header or host list, or deleting the credential,
supersedes the credential's pending and approved approvals in the same
transaction, so an approval never covers a header or hosts the approver did
not see. A superseded approval cannot be approved. The queue and the
standing approvals show the requested host and the credential's current
header and hosts.

Every request, decision, use, and change to credentials, assignments and
approvals adds an audit row. The broker also prints each row as one JSON
line on stdout.

## Design

| Namespace | Role |
|-----------|------|
| `ts-broker.domain` | Pure rules: request parsing, host patterns, the transition table, `decide` |
| `ts-broker.store` | SQLite in WAL mode. One writer thread runs queued commands in batches, one transaction per batch and one savepoint per command. A pool of reader connections serves reads. |
| `ts-broker.socket` | NDJSON decide server on the unix socket |
| `ts-broker.connect` | Read-only Connect catalog. Field values are dropped. |
| `ts-broker.http` | Datastar server. Each page opens one SSE stream, which re-renders from the database after each committed write. |
| `ts-broker.lib` | `defview` and `defaction`: pages and actions as vars that carry their paths |
| `ts-broker.pages` | Pages (approvals, credentials, catalog, assign, identities, audit) |
| `ts-broker.actions` | Actions, each served at `/action/<name>` |
| `ts-broker.main` | Command line |

## Development

The REPL and the tests need `libsqlite3.so.0` on `LD_LIBRARY_PATH`.

```sh
bb test
bb lint
bb dev        # nREPL; (ts-broker.dev/start! dir port) runs a seeded broker
              # against a fake Connect server
```

After changing `deps.edn`, run the package's `locker` passthru from this
directory to update `deps.lock.json`. `nix build .#ts-broker` runs the
tests. `nix build .#checks.x86_64-linux.ts-broker` runs the NixOS VM test.
