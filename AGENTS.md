# AGENTS.md

The authoritative instructions for the `nixcfg` and `nixcfg-private` repository pair are in `~/nixcfg-private/AGENTS.md`.
Read and follow that file before work in this repository.

Default all configuration, host assignments, topology, tests, and documentation to this public repository unless they contain an actual secret value or the human explicitly classifies them as private.

When adding a host to chained Zrepl replication, exclude snapshotless receive-root containers from onward exports and explicitly include the backed-up descendants. Check selection with `zrepl test filesystems` and verify successful replication at the final receiver; a successful first hop does not prove offsite coverage.
