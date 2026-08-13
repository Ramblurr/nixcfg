---
name: dns-admin-ui
description: Start, stop, test, or modify the local DNS record editor in scripts/dns-admin-ui.clj. Use when asked about updating/managing DNS admin ui.
---
Run the editor from ~/nixcfg with ./scripts/dns-admin-ui.clj.
It serves http://127.0.0.1:8083/ and reads ~/nixcfg-private/terranix/dns/zones by default.
Use tmuxb for the server; capture before sending commands, verify /health after starting, and stop it with Ctrl-C.

Before editing Clojure, establish brepl and follow the Clojure, Datastar, and testing skills.
Keep scripts/dns-admin-ui.clj as one executable file.
Never change anything between the framework markers; verify that region byte-for-byte against ~/src/github.com/ramblurr/playground/babashka-datastar/bbweb.clj.

After edits run ./scripts/dns-admin-ui.clj --test, cljfmt check scripts/dns-admin-ui.clj, clj-kondo --lint scripts/dns-admin-ui.clj, git diff --check, and a focused browser check.
The editor may only edit zone JSON files; do not add provider, DNS inventory, Nix, OpenTofu, Git, or subprocess behavior.
