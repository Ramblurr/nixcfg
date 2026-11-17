# Caddy with deSEC and caddy-security

`caddy-with-security` extends the nixpkgs Caddy package with the imports in
`src/main.go`. Caddy and dependency versions are recorded in `src/go.mod` and
`src/go.sum`. The inherited `buildGoModule` builder fetches and vendors that
locked dependency graph using `vendorHash`; normal builds do not run xcaddy,
`go mod init`, or dependency updates.

We inherit nixpkgs's build tags, version flags, service files, man pages, shell
completions and version-check hook. Installation checks also verify the exact
Caddy/plugin module versions against `go.mod` and the presence of deSEC,
authenticator, authorizer and security modules. Evaluation refuses a mismatch
between the locked Caddy module version and the nixpkgs Caddy package version.

Every host builds this package from the unstable channel's Caddy, stable hosts
included (see `pkgs/default.nix`). In November 2026 unstable moved to the Caddy
2.11.9 security release while its backport to the stable channel failed; one
`go.mod` cannot match two Caddy versions.

## Why the module files are checked in

The former `withPlugins` build generated `go.mod` inside a fixed-output source
builder. `go mod init` writes the generator's Go version into that file, so a Go
patch update could change the source hash without changing Caddy or a plugin.

In August 2026, different host channels generated different source bundles with
Go 1.26.4 and 1.26.5. Sharing Go and xcaddy from one nixpkgs input fixed that
cross-host collision. In September, that shared input advanced from Go 1.26.4 /
xcaddy 0.4.5 to Go 1.26.5 / xcaddy 0.4.6, but the source hash remained unchanged.
Sharing tools had not made generated source independent of tool updates.

Keeping the module files in Git removes that generation step from builds.
Dependency updates are explicit and reviewable. This does not promise that all
future Go changes preserve the vendor format or build compatibility; intentional
dependency changes still require a reviewed vendor hash update.

## Update Caddy or a plugin

Run from `~/nixcfg`. Use the repository's Go toolchain, not an arbitrary ambient
Go version:

```sh
nix shell .#legacyPackages.x86_64-linux.go --command bash
cd pkgs/caddy/src
export GOTOOLCHAIN=local GOWORK=off GOENV=off
```

Run `go get` with explicit selected versions. For example, these are the current
pins; change only the version(s) you intend to update:

```sh
go get github.com/caddyserver/caddy/v2@v2.11.9 \
  github.com/caddy-dns/desec@v1.1.0 \
  github.com/greenpau/caddy-security@v1.6.2
go mod tidy
go mod verify
```

Do not delete the manifests or use `@latest` / `go get -u` for routine updates.
Review `main.go`, `go.mod` and `go.sum`, including any Go language/toolchain
changes. When adding/removing a plugin, update the installation checks too.
Caddy updates must match the unstable nixpkgs package version; do not bypass the version
assertion or plugin checks to make a build pass.

From the repository root, temporarily set `vendorHash` in `package.nix` to
`lib.fakeHash`, then build the package:

```sh
nix build --no-link -L .#caddy-with-security
```

Review the expected vendor-hash mismatch and replace the placeholder with the
reported hash. Build again and require the installation checks to pass. Run the
package check too:

```sh
nix build --no-link -L .#checks.x86_64-linux.caddy-package
```

It checks that stable hosts get the same derivation as unstable hosts, that the
source manifests are unchanged, the installed packaging files, and that a Caddy
version mismatch is rejected.
Commit the module files and vendor hash together. This workflow does not change
global Git configuration, commit, push or deploy automatically.

For real host validation, commit the public changes, coordinate a clean tracked
public tree, update only the private wrapper's `nixcfg` input, then run from
`~/nixcfg-private`:

```sh
nix develop --command build debord dewey mali
```

Deployment is a separate operational step.
