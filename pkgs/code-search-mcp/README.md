# Code-search MCP

`pkgs/code-search-mcp/code-search-mcp.clj` serves read-only MCP over the existing Neogrok
index and bare Git mirrors. This directory pins its dependencies. There is no
deployed MCP route yet. The offline Nix package builds successfully and runs the
full test suite in Nix's sandbox.

## Run locally

Use Babashka 1.13.226 or newer and Git. On NixOS, provide the native SQLite
library explicitly:

```sh
export LD_LIBRARY_PATH="$(nix build --no-link --print-out-paths nixpkgs#sqlite.out)/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
PORT=18765 \
CODE_SEARCH_ORIGIN=http://127.0.0.1:18765 \
NEOGROK_URL=https://src.example.net/search \
MIRROR_ROOT=/path/to/bare-mirrors \
STATE_DIRECTORY="${XDG_STATE_HOME:-$HOME/.local/state}/code-search-mcp-dev" \
bb pkgs/code-search-mcp/code-search-mcp.clj
```

Replace the backend hostname and mirror path. The endpoint is
`http://127.0.0.1:18765/mcp`; it always binds loopback. The state directory must
be dedicated to this service. Mirrors are read but never modified or fetched.
Omit `MIRROR_ROOT` and `STATE_DIRECTORY` for search-only mode, which exposes
only `grep` and `list_repos`.

Startup resolves pinned development dependencies and may need Java and network
access. The SDK is pinned to the Forgejo fork's reviewed capability-control
commit. The Nix package supplies an offline classpath instead.
SQLite uses experimental native FFI, not a separate pod process.

## Tools and revisions

With a mirror root configured, all ten tools are available:

- `grep`: Zoekt query syntax, bounded matches and context.
- `list_repos`: published mirror/index observations and freshness warnings;
  its optional query is a literal repository-ID substring and its limit defaults to 50. In search-only mode,
  it retains Neogrok's indexed-repository list and Zoekt query semantics.
- `glob`: case-sensitive `*` and `?` within path components; a whole `**`
  component spans zero or more directories. Other punctuation is literal.
- `find_symbol_definitions`: exact, case-sensitive Ctags names.
- `find_symbol_references`: case-sensitive textual name occurrences with
  alphanumeric/underscore boundaries, **not semantic references**.
- `read_file`: UTF-8 text, inclusive one-based line ranges, at most 500 lines.
  Git symlinks return link text rather than following the target.
- `list_tree`: immediate directory children at a resolved revision.
- `list_branches`: the published branch snapshot, without per-request Git work.
- `list_commits`: bounded history. Messages, authors and unusually large parent
  lists have explicit truncation flags.
- `get_diff`: bounded diff, with external diff drivers and textconv disabled.

Repository IDs match Neogrok, for example `github.com/owner/project`, without
`.git`. Git tools accept `HEAD`, an exact published branch name (optionally
prefixed with `refs/heads/`), or a lowercase full commit ID. Tags and arbitrary
Git revision expressions are not supported. Paths are literal, repository-relative
paths; traversal is rejected.

Search results preserve backend field names and cgit URLs. An immutable `id`
from the file URL is also exposed as `revision`; otherwise a `revisionWarning`
is returned. Use that exact revision for search-driven file reads, not HEAD.
An unavailable object produces an error rather than substituting a newer commit.

## Refresh, cache and limits

Startup prepares a catalog; one background worker refreshes it every five
minutes. Each repository's refs and tree publish in one SQLite transaction,
after Git traversal finishes. Unchanged tree objects are not traversed again,
including when HEAD changes without changing its tree. Failed
refreshes retain the previous generation with a warning. Index observations are
separate from mirror HEAD and can lag it.

Prepared default-branch metadata needs no Git subprocess on a read. Historical
trees, history pages and diffs use an immutable-result LRU cache. Cached payload
files have a 16 MiB aggregate budget, a 1 MiB per-entry limit and a 1,024-entry
limit. Catalog SQLite/WAL files and cache metadata are separate from that payload
budget. Cache writes are serialized; simultaneous cold misses may compute twice.
File contents are not cached.

Requests share four work slots. Git, backend requests and glob/metadata walks
use a ten-second request budget; SQLite calls also have their own bounded busy
wait, so this is not a hard wall-clock limit for every database operation. Git stdout
is capped at 8 MiB, stderr at 8 KiB; backend and tool-result payloads at 2 MiB.
Over-limit work fails rather than caching a partial result. Timed-out Git work
is killed, including observed descendants. SQLite connections are private to an
operation and use the library's bounded busy timeout.

## Nix package

The `code-search-mcp` package copies the locked dependency classpath into its
output and supplies Git and the native SQLite library. Its launcher disables
runtime dependency resolution and ignores the working directory's `bb.edn`.
The build runs the full test suite with an empty test home directory.

The installed launcher also passed a separate TCP initialization and ten-tool
listing check with an empty home and no Java or download tools on `PATH`.
The standard Babashka package still brings its optional Clojure/JDK tooling into
the Nix closure; the service does not invoke it.

To update the lockfile, run the package's `locker` passthru from this directory.
It uses a separate temporary dependency home. Build and test the package again
after any dependency update.

## Test

With `LD_LIBRARY_PATH` set as above:

```sh
bb tests/code-search-mcp_test.clj
clj-kondo --lint pkgs/code-search-mcp/code-search-mcp.clj tests/code-search-mcp_test.clj
```

Tests use real TCP MCP, an HTTP backend fixture, native SQLite and temporary bare
Git repositories. They cover all ten tools, immutable reads, literal paths and
Unicode, refresh publication, cache eviction, subprocess bounds, cancellation,
protocol/session handling and Host/Origin checks. Manual probes also exercise
real Neogrok symbol searches. Small-fixture cold/warm measurements live in local
work item `099-code-search-mcp`; they are not production performance claims.

Supported handshake revisions are 2025-06-18 and 2025-11-25. POST requests use
SSE responses; notifications return 202. GET returns 405. Stateless revisions,
replay, server-initiated requests, a query CLI and a separate HTTP API are excluded.

## NixOS service

Enable `modules.services.code-search-mcp.enable` alongside dependency mirrors
and Neogrok. The service listens only on `127.0.0.1:3082`. Caddy adds `/mcp`
without changing the existing cgit, editor or search handlers.

`allowedRemoteIPs` permits loopback and Tailscale clients by default. Peirce
also permits its primary and management LANs. The route checks the direct peer
address, not caller-supplied forwarding headers. The approved initial policy
uses no agent token. No firewall port is added for the application listener.

The separate service user can read mirrors but cannot write them. Its writable
catalog/cache is `/var/lib/code-search-mcp`, with private temporary storage.
This state is derived and can be rebuilt; this module does not add persistence
or backups for it. The service has a 1 GiB memory limit, a two-CPU quota and a
128-task limit. Outbound IP traffic is restricted to loopback.

The VM check uses the real systemd sandbox, Caddy, Neogrok and Zoekt. It calls
all ten MCP tools, rejects a client outside the allowlist even with a spoofed
forwarding header, and verifies that the mirror mount rejects writes. cgit and
editor route bodies are fixtures in this test; real service health must still
be checked after deployment.

## Before production

- Finish independent review of the deployment configuration.
- Keep the offline package and VM checks passing when dependencies change.
- Obtain explicit target deployment approval and record rollback/validation.

Host/Origin validation is not authentication. Do not expose this development
server publicly.
