# Mali rsync.net zrepl reconciler

This is the non-secret Mali side of work item 035-04. A disabled-by-default NixOS module installs a dedicated unprivileged systemd one-shot and a persistent 15-minute timer. Each run opens one strictly pinned SSH session with no requested remote command. The receiver's provider-managed authorized-key entry must be equivalent to:

```text
restrict,from="<literal Mali egress CIDR>",command="/mnt/local/zrepl-recovery/current/bootstrap.sh" <dedicated public key>
```

Compatibility options must also explicitly disable agent, port, and X11 forwarding, PTY allocation, and user rc processing. The identity cannot select another executable, transfer a file, or open a shell.

The one-shot is pinned to the accepted recovery bundle `v1-c337a0f46626b904-initial` and accepts only its exact state/reason/change/validation/exit matrix plus bounded `ZREPL_SNAPSHOT_V1` records. It never replays unvalidated stdout or SSH stderr. Healthy and active catch-up results are no-ops. Receiver continuity/planning errors are reported without a second command. Nonzero fail-safe `CATCHING-UP changed=1 validation=fail` results preserve both activity and mutation evidence. Validated receiver lock contention remains exit 75 with `action=retry` and does not change the consecutive-failure counter. SSH has a module-fixed 15-minute process deadline below the 20-minute systemd timeout, so hangs become persisted bounded timeout failures. HUP, INT, and TERM stop the bounded SSH process and persist exact `reason=terminated action=retry` failure/threshold evidence before exiting with the conventional signal status.

A successful repair records the three bounded snapshot markers and waits up to 45 minutes for at least one newer creation time; expiration reports an error without waking or changing replication. The baseline, deadline, last result, last success, and failure counter live in `/var/lib/rsyncnet-zrepl-reconcile`. The Mali module backs that path with `rpool2/encrypted/safe/svc/zrepl-reconcile`, mounted at `/var/lib/private/rsyncnet-zrepl-reconcile`; systemd `DynamicUser` and `StateDirectory` provide the runtime identity and mode `0700`.

## Private wiring and enablement gate

`services.rsyncnet-zrepl-reconcile.enable` may install the operator-gated one-shot only after private configuration supplies all of the following and the provider prerequisites have been independently accepted. `services.rsyncnet-zrepl-reconcile.timer.enable` must remain `false` through deployment and the approved first execution:

- `receiverHost`;
- `identityReference`, pointing to Mali's dedicated SSH private key in 1Password;
- `knownHostsReference`, pointing to an authenticated receiver `known_hosts` line in 1Password;
- provider-managed installation of the matching public key with the exact forced-command and source-address restrictions.
The 1Password systemd credential provider owns `LoadCredential` injection for credential IDs `identity` and `known-hosts`. The reconciler reads only `$CREDENTIALS_DIRECTORY/identity` and `$CREDENTIALS_DIRECTORY/known-hosts`; it does not resolve `op://` references or configure a second credential source.
The dedicated SSH identity and known-host entry are Mali-owned. They are not the receiver TLS identity. Mali must never receive, materialize, read, transmit, or install `rsyncnet.key`. If the persistent receiver bundle or its receiver-local key is absent, the bounded result requests human restoration from 1Password and fails closed.

Repository implementation is not deployment authorization. Enabling the timer, materializing its Mali credentials, provisioning the provider key, or invoking the receiver requires independent review and a separate approved OCP ledger.

## Focused validation

```sh
./tests/run.sh
```
