# harness-plugins-sync

CLI that syncs Git agent plugin packs into immutable snapshots and binds them
natively across harnesses (`claude`, `codex`, `pi`, `antigravity`) and provider
profiles (including T3 Code `providerInstances`), with SSH fanout to peer hosts.

## Directories

- Config: `${XDG_CONFIG_HOME:-~/.config}/ol-plugins-sync/config.edn`
- State: `${XDG_DATA_HOME:-~/.local/share}/ol-plugins-sync/`

## Workflow

```sh
harness-plugins-sync sync                  # fetch + promote + bind this host
harness-plugins-sync sync --all            # sync locally and fan out to peers
harness-plugins-sync pull                  # fetch into staged snapshots only
harness-plugins-sync apply                 # promote staged snapshots and bind
harness-plugins-sync apply --local harness-plugins=~/src/git.outskirtslabs.com/casey/skills
harness-plugins-sync status --json         # inspect revisions and drift
harness-plugins-sync doctor                # read-only environment checks
```

## Packs

A pack is any Git repository with a Codex marketplace at
`.agents/plugins/marketplace.json`. Each plugin binds from the local source path
its marketplace entry names, so a repository that is one plugin at its root lists
it at `./`. A plugin the marketplace does not list binds from `plugins/<name>`.
On Pi, a plugin's `package.json` `pi` manifest selects its resources, and a plugin
without one contributes its `skills/` directory.

Codex registers each pack as a marketplace named after its config key, from a
copy of its plugins that the tool generates per revision. The upstream
marketplace name never reaches Codex, so two packs cannot claim one name, and
pruning old snapshots never removes a directory that Codex still reads. A pack
removed from the config is unregistered before its copy is deleted.

Each new snapshot is checked before it becomes active. A pack that fails to
fetch, or whose snapshot has an unusable marketplace or a plugin path that
resolves outside the pack, keeps its last good snapshot. A pack with no good
snapshot is left out of each profile, which still binds its other packs. Either
case marks the host not current.

## Config (`config.edn`)

Configuration is read as EDN data (never evaluated). Without a config file,
built-in defaults sync `harness-plugins` from
`https://git.outskirtslabs.com/casey/skills.git` (`main`) and discover T3 Code
provider instances automatically.

```clojure
{:state-dir   "~/.local/share/ol-plugins-sync"
 :t3-settings "~/.config/t3/userdata/settings.json"
 :peers       ["quine" "witt" "thingstead"]
 :packs       {"harness-plugins"
               {:url    "https://git.outskirtslabs.com/casey/skills.git"
                :branch "main"}}
 :defaults    {:plugins ["my@harness-plugins"
                         "pstack@harness-plugins"
                         "tstack@harness-plugins"]}
 :profiles    {"pi" {:harness :pi :dir "~/.config/pi/agent"}}}
```

## Development & Tests

```sh
./harness-plugins-sync help
./harness-plugins-sync status
bb build         # regenerate standalone ./harness-plugins-sync uberscript
bb test
bb lint
```
