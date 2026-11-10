# Code-search MCP

`pkgs/code-search-mcp/code-search-mcp.clj` serves MCP over the existing Neogrok
index and bare Git mirrors, plus two tools that add repositories to the mirror
inventory and start syncs. This directory pins its dependencies. The offline Nix
package runs the full test suite in Nix's sandbox.

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

Replace the backend hostname and mirror path. The endpoints are
`http://127.0.0.1:18765/mcp` and `/mcp/all`; the server always binds loopback.
The state directory must be dedicated to this service. Mirrors are read but
never modified or fetched by this process. Omit `MIRROR_ROOT` and
`STATE_DIRECTORY` for search-only mode, which exposes only `grep` and
`list_repos`. Set `EDITOR_URL` (for example `http://127.0.0.1:8084/edit`) to
enable `upsert_repos` and `sync_repos`. `CODE_SEARCH_DISABLED_TOOLS` is a
comma-separated list of tool names to remove; unknown names stop startup.

Startup resolves pinned development dependencies and may need Java and network
access. The SDK is pinned to the Forgejo fork's reviewed capability-control
commit. The Nix package supplies an offline classpath instead.
SQLite uses experimental native FFI, not a separate pod process.

## Endpoints

- `/mcp`: core tools `grep`, `read_file`, `list_files`, `list_repos`,
  `commit_search`, `upsert_repos`, `sync_repos`.
- `/mcp/all`: every tool, adding `glob`, `list_refs`, `compare_revisions`,
  `find_symbol_definitions` and `find_symbol_references`.

A session belongs to the endpoint that created it. The server sends a short
`instructions` string at initialization describing the workflow.

## Result contract

Every tool takes `format`: `"edn"` (the default) or `"json"`. Both carry the
same data; JSON keys are the camelCase form of the EDN keywords
(`:has-more` becomes `hasMore`). Nil values, false flags and empty collections
are omitted. Results are maps with these common keys:

- `:results`: the rows.
- `:has-more`: more rows exist than were returned.
- `:hint`: one sentence naming the argument to change.
- `:next`: a paging cursor, where the tool pages.
- `:total`: the total row count, when cheaply known.
- `:warnings`: present only when non-empty.

`grep`, `read_file`, `commit_search`, `compare_revisions` and the symbol tools
take `maxChars` (default 20,000). Oversized results drop trailing rows or lines
and say so in `:hint`; a line is never cut mid-way, and lines over 2,000
characters end with ` [truncated]`. The 2 MiB response cap still applies.

Search rows carry `:cite`, `repo:path@commit:start-end`, with the full commit
ID. `read_file` accepts `cite` in place of repository, path, revision and line
arguments. The cite range starts at the first shown line and spans at most 200
lines. Argument errors name the offending argument.

## Tools and revisions

- `grep`: Zoekt query syntax; the description summarizes it. Defaults: 10
  files, 50 matches, no context. Rows are `{:repo :path :lang :cite :lines}`
  with `[line text]` pairs in file order; with `contextLines`, `:matched` lists
  the matching line numbers. `facets: true` adds file counts by repository,
  language, top-level directory and extension from one extra file-name-only
  (`type:file`) search over up to 500 files. Each facet value carries a Zoekt
  exclusion token, such as `-repo:^github\.com/a/b$`, to append to the query.
- `list_repos`: `query` is required; it is a case-insensitive repository-ID
  substring, and `""` or `"*"` lists everything. Rows are
  `{:id :last-commit :files}` plus `:warnings` for refresh failures, missing
  mirrors, missing index data or an index revision that differs from mirror
  HEAD. `sort` is `name` or `recent`; `problems: true` keeps only rows with
  warnings. Default limit 50. In search-only mode, the query is Zoekt syntax.
- `read_file`: UTF-8 text, inclusive one-based line ranges, default 200 and at
  most 500 lines. The result reports `:start`, `:end` and `:total-lines`, and
  its hint gives the next `startLine`. Git symlinks return link text and set
  `:symlink`.
- `list_files`: entries below `path`, relative to it, directories ending in
  `/`. `depth` defaults to 1; `kind` is `all`, `files` or `dirs`.
- `glob`: case-sensitive `*` and `?` within path components; a whole `**`
  component spans zero or more directories. Other punctuation is literal.
- `list_refs`: published branches and tags. `headsOnly`, `tagsOnly` and
  `containsCommit` filter them; the last runs one bounded
  `git for-each-ref --contains`.
- `commit_search`: Sourcegraph-style filters across up to ten repositories.
  `messageTerms` must all occur (case-insensitive); any of `authors` may match;
  `contentTerms` are literal strings a commit added or removed (`git log -G`);
  `files` are touched paths; `after`/`before` accept `YYYY-MM-DD`, ISO times,
  `N days ago`, `yesterday` or `today`. Absolute dates go to Git as epoch
  seconds because Git silently ignores some dates it cannot parse.
  Per-repository failures become warnings. Date-free queries are cached.
- `compare_revisions`: changed files between two revisions with status,
  added/deleted counts and hunks; pages with `first` and `after`. Patches over
  20,000 characters are cut with a note, and files with more than 20,000 changed
  lines get a placeholder instead of a patch. A path that is also a directory
  prefix of another changed path is diffed on its own. External diff drivers, textconv and
  rename detection are disabled.
- `find_symbol_definitions`: exact, case-sensitive Ctags names.
- `find_symbol_references`: case-sensitive textual name occurrences with
  identifier boundaries. These are not semantic references.
- `upsert_repos`: creates or updates dependency-mirror inventory entries through
  the editor's JSON Merge Patch API. It accepts every editor option: `include`,
  `provider`, `scope`, `organization`, `issues`, `wiki`, `contributed`, `gists`
  and `excludeForks`. For an existing entry, given options replace current
  values (`include` replaces the whole list), omitted options are kept, and
  `unset` removes options to restore defaults. Entries that would not change
  are not written. The result lists created, updated and unchanged canonical
  URLs. URLs are normalized like the editor does: the host is lowercased,
  trailing slashes are removed, a final `.git` is removed except for plain Git
  URLs (an unknown host with no `provider`), and github.com paths are
  lowercased. The editor validates provider support and rejects unsupported
  options with its own message. Patches are split below the editor's 16 KiB
  request limit; each request is atomic, but the inventory read and the
  patches are separate requests. The tool description documents URL forms,
  providers, option support and examples for agents. It does not delete
  entries.
- `sync_repos`: starts a background sync for one inventory URL or `all: true`.
  It returns when the editor accepts; a running sync is reported as busy.

Repository IDs match Neogrok, for example `github.com/owner/project`, without
`.git`. Git tools accept `HEAD`, a published branch or tag name (branches win
over tags with the same name; `refs/heads/` and `refs/tags/` prefixes are
accepted), or a lowercase full commit ID. Annotated tags resolve to the commit
they point at. Arbitrary Git revision expressions are not supported. Paths are
literal, repository-relative paths; traversal is rejected.

Search citations use the immutable commit from the backend's file URL. When the
backend supplies none, the row has no cite and the result carries a warning.
An unavailable object produces an error rather than substituting a newer commit.

## Refresh, cache and limits

Startup prepares a catalog; one background worker refreshes it every five
minutes. Each repository's branches, peeled tags and tree publish in one SQLite
transaction, after Git traversal finishes. Pull-request and other refs are not
published. Unchanged ref lists are not rewritten. A repository with too many
refs to list within the 8 MiB Git output cap publishes its branches only, with
a warning, rather than failing every refresh. Unchanged tree objects are not traversed again, including when HEAD
changes without changing its tree. Failed refreshes retain the previous
generation with a warning. Index observations are separate from mirror HEAD and
can lag it.

Prepared default-branch metadata needs no Git subprocess on a read. Historical
trees, commit searches and comparisons use an immutable-result LRU cache. Cached
payload files have a 16 MiB aggregate budget, a 1 MiB per-entry limit and a
1,024-entry limit. Catalog SQLite/WAL files and cache metadata are separate from
that payload budget. Cache writes are serialized; simultaneous cold misses may
compute twice. File contents are not cached.

Requests share four work slots. Git, backend requests and glob/metadata walks
use a ten-second request budget; each mirror editor request has its own
ten-second timeout; SQLite calls also have
their own bounded busy wait, so this is not a hard wall-clock limit for every
database operation. Git stdout is capped at 8 MiB, stderr at 8 KiB; backend and
tool-result payloads at 2 MiB. Over-limit work fails rather than caching a
partial result. Timed-out Git work is killed, including observed descendants.
SQLite connections are private to an operation and use the library's bounded
busy timeout.

## Nix package

The `code-search-mcp` package copies the locked dependency classpath into its
output and supplies Git and the native SQLite library. Its launcher disables
runtime dependency resolution and ignores the working directory's `bb.edn`.
The build runs the full test suite with an empty test home directory.

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

Tests use real TCP MCP, an HTTP backend fixture, a fixture mirror editor,
native SQLite and temporary bare Git repositories. They cover every tool in both
formats, both endpoints and disabled tools, the size budget, citations, refs and
tags, commit filters, comparison paging, facets, inventory changes, immutable
reads, literal paths and Unicode, refresh publication, cache eviction,
subprocess bounds, cancellation, protocol/session handling and Host/Origin
checks.

Supported handshake revisions are 2025-06-18 and 2025-11-25. POST requests use
SSE responses; notifications return 202. GET returns 405. Stateless revisions,
replay, server-initiated requests, a query CLI and a separate HTTP API are
excluded.

## NixOS service

Enable `modules.services.code-search-mcp.enable` alongside dependency mirrors
and Neogrok. The service listens only on `127.0.0.1:3082`. Caddy routes `/mcp`
and `/mcp/*` without changing the existing cgit, editor or search handlers.

`allowedRemoteIPs` permits loopback and Tailscale clients by default. Peirce
also permits its primary and management LANs. The route checks the direct peer
address, not caller-supplied forwarding headers. There is no agent token, so
any allowed client can also add inventory entries and start syncs.
`disabledTools` removes named tools, for example `[ "sync_repos" ]`. No firewall
port is added for the application listener.

The separate service user can read mirrors but cannot write them. Inventory
changes go through the editor on loopback (`EDITOR_URL`, derived from the
dependency-mirror module), which owns the inventory file and runs syncs. Its
writable catalog/cache is `/var/lib/code-search-mcp`, with private temporary
storage. This state is derived and can be rebuilt; this module does not add
persistence or backups for it. The service has a 1 GiB memory limit, a two-CPU
quota and a 128-task limit. Outbound IP traffic is restricted to loopback.

The VM check uses the real systemd sandbox, Caddy, Neogrok and Zoekt. It calls
every MCP tool on both endpoints, rejects a client outside the allowlist even
with a spoofed forwarding header, and verifies that the mirror mount rejects
writes. Route bodies for cgit are fixtures in this test; real service health
must still be checked after deployment.

Host/Origin validation is not authentication. Do not expose this server
publicly.
