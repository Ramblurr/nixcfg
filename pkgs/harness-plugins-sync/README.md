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
bb test
clj-kondo --lint src test
```
