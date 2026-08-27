# rsync.net zrepl recovery bundle

This directory is the non-secret source for the persistent receiver recovery bundle accepted in work item 035. `publish-release.sh` runs only on the receiver: it installs an immutable-by-contract release under `/mnt/local/zrepl-recovery/releases/`, verifies separate repository, public-certificate, and silent private-key manifests, and atomically repoints `current` only after complete release validation. It does not install live files or start zrepl. `bootstrap.sh` repairs only process-free `ABSENT` or `PARTIAL` disposable installations.

Package recovery installs the current `zrepl` package from the configured FreeBSD repository. The bundle does not pin a package version; it still validates the transaction scope, package name, origin, architecture, scripts, payload, and owned executable/rc paths. Bootstrap installs the exact archive it inspected, so a repository update cannot swap candidates between validation and installation.

## Receiver-local private key boundary

Private key material never enters Git, SOPS, Mali, the public transfer archive, command arguments, stdout, or logs. Publication and bootstrap may use surviving root-only receiver-local copies without exposing their contents or private manifest hash:

- the publisher silently selects matching valid copies from `/mnt/local/etc2/rsyncnet.key` and an existing validated `current` release, then writes mode `0600` release storage;
- bootstrap silently verifies `current/tls/rsyncnet.key`, backs up and projects it only between root-only receiver-local paths, and never emits its contents or checksum;
- Mali can invoke only the fixed receiver entrypoint and never receives, reads, materializes, or transmits the key.

If no valid receiver-local persistent copy survives, publication/bootstrap fails closed. A human must restore a lost key manually from 1Password before retrying; the key is never recovered through SOPS or Mali.

The publisher also copies the two non-secret retained certificates from fixed receiver-local paths. Dedicated SSH identity, pinned host key, provider-managed authorized-key provisioning, and account-specific wiring remain explicit handoff gaps. Unattended reconciliation stays disabled until those prerequisites are established.

Do not deploy or invoke bootstrap from repository work alone. Receiver mutation requires an exact OCP ledger, committed canonical inputs, independent review, and explicit operator GO.

## Local validation

```sh
./tests/run.sh
```
