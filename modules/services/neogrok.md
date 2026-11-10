# Search dependency mirrors with Neogrok

Peirce serves Neogrok at `http://10.9.4.24:3081/` on the LAN. Zoekt's backend
listens only on `127.0.0.1:6070`. Neither service has application authentication;
do not expose these listeners publicly. Sourcebot's separate trial is unchanged.

## Indexing

`neogrok-index.timer` runs two minutes after boot and five minutes after the
previous indexing job finishes. It recursively discovers retained `*.git`
directories in `/var/lib/dependency-mirror/repos`, indexing their default branch
with Universal Ctags. Empty or unfinished clones are skipped until a later run.
Failures on complete repositories make the job fail after attempting the rest.

The indexer uses private Git metadata under `/var/cache/neogrok/repos` with shared,
read-only objects and refs. This supplies stable repository names and cgit links
without modifying Gickup's repository configurations or duplicating clones.
`/var/cache/neogrok/index` contains disposable Zoekt shards; cache loss after an
impermanent-root reboot triggers rebuilding. No new backup dataset is needed.

Adding an entry in `/edit/` does not immediately make it searchable: Gickup must
clone it, then the indexer must run. Removing an inventory entry keeps its mirror,
so it remains searchable. Physical repository removal and stale-shard cleanup
are not automated by this module. LFS objects and submodules are not indexed.

```sh
ssh peirce 'sudo systemctl start neogrok-index'
ssh peirce 'systemctl status neogrok zoekt neogrok-index --no-pager'
ssh peirce 'sudo journalctl -u neogrok-index -n 50 --no-pager'
```

## API and symbols

```sh
curl -fsS http://10.9.4.24:3081/api/list \
  -H 'Content-Type: application/json' -d '{}'
curl -fsS http://10.9.4.24:3081/api/search \
  -H 'Content-Type: application/json' \
  -d '{"query":"repo:gickup sym:main","contextLines":2,"files":10,"matches":20}'
```

The APIs return `kind: success` or `kind: error`; check the body as well as HTTP
status. `sym:` searches Ctags definitions. Word-boundary regex searches can find
name occurrences, but do not provide compiler-accurate reference resolution.
There is no built-in MCP server or search CLI. Search-result links open cgit at
the indexed commit and line.

## Package and verification

`pkgs/neogrok/package.nix` builds pinned upstream source with Node 24 and Yarn 4,
checks upstream tests, and installs only production dependencies. The small
`cgit-url-templates.patch` supports the query-string suffix in Zoekt's cgit URL
templates. Run `bb pkgs/neogrok/update.bb` from the public checkout to update to the latest
release, or pass a tag/commit explicitly. It refreshes source, dependency cache,
and missing platform hashes together and builds the package with upstream tests.
A failed build restores the original package and hashes; review patch compatibility
before retrying.

`tests/neogrok.nix` exercises the actual packaged frontend and nixpkgs Zoekt with
bare Git fixtures: same-name repositories, empty repositories, Ctags search,
cgit result URLs, unchanged source Git configuration, and incremental updates.
