# Static site configuration

`modules.services.staticSites.sites.<name>` declares an rsync-deployed static site.
James consumes these declarations through its Caddy and HAProxy configuration.
Other hosts need an ingress adapter consuming `handlersByDomain`.

```nix
modules.services.staticSites.sites.handbook = {
  domain = "handbook.example.com";
  authorizedKeys = [ "ssh-ed25519 ..." ];
};
```

## Options

| Option | Default | Meaning |
|---|---|---|
| `enable` | `true` | Serve the site and allow deployments. `false` removes its routes and keys and denies SSH commands, retaining its account and files. |
| `domain` | Required | Exact lowercase DNS hostname already pointing at James. |
| `path` | `"/"` | URL mount with leading and trailing slashes. |
| `deployUser` | `"<name>-deploy"` | Distinct account for this site; duplicates, including disabled sites, are rejected. |
| `authorizedKeys` | Required | Public SSH keys. Private keys stay with CI. |
| `directory` | `"/var/lib/static-sites/<name>"` | Deployment directory. The default tree is persisted under `/persist`. Overrides require an existing persistent mount and safe parent permissions. |
| `spaFallback` | `false` | Rewrite missing files to `/index.html` within this site. |
| `cacheControl` | `"public, max-age=0, must-revalidate"` | Default Cache-Control value; `null` leaves the header unset. |
| `cacheControlByMimeType` | `{ }` | Exact MIME or `type/*` overrides, independent of charset parameters. |
| `cacheControlByPath` | `{ }` | Exact path or `/prefix/*` overrides. Paths are relative to the site's mount, after SPA fallback. |

Root redirects are separate Caddy configuration. A root mount cannot replace an
existing James route. Multiple non-overlapping subpath sites can share a hostname.
Each hostname has one certificate; new hostnames are routed to the loopback
TLS-ALPN listener automatically. HTTP/3 is unchanged.

## Deployment command

```sh
rsync -av --delete --chmod=D755,F644 public/ handbook-deploy@handbook.example.com:.
```

The destination `:.` means the site's deployment directory, not the account's home.
Absolute destinations inside that directory also work, preserving existing epsilon
CI commands. The packaged rrsync rejects `:./`; use `:.`.

SSH permits only write-side rsync. Shell commands, downloads, forwarding, and writes
outside the site's directory are denied. Symlinks are munged and are not usable as
published links; deploy real files. The account's home is the non-writable
`/var/empty`, separate from uploaded content. All published files are public,
including dotfiles. Uploads are not atomic releases.

Disabling or removing a declaration never deletes content. `enable = false`
retains the account and explicitly denies SSH access; account cleanup after removal
follows the host's NixOS user-management policy. Retain `/var/lib/nixos` across
reboots to preserve dynamic UID/GID assignments, as James already does.

## Cache selection

Successful file responses use the first applicable policy:

1. Exact path.
2. Longest matching `/prefix/*`.
3. Exact MIME type.
4. MIME wildcard.
5. Site-wide `cacheControl`.

```nix
cacheControlByMimeType = {
  "text/css" = "public, max-age=3600";
  "image/*" = "public, max-age=86400";
};
cacheControlByPath = {
  "/assets/*" = "public, max-age=31536000, immutable";
  "/assets/runtime-config.json" = "public, max-age=0, must-revalidate";
};
```

`immutable` requires content-versioned URLs for every matching file. MIME overrides
do not detect hashed filenames. SPA fallback selects policies for `/index.html`,
not the missing asset URL. Errors and redirects receive no module-added caching
header. ETag and Last-Modified remain native Caddy behavior; 304 responses retain
the client's stored caching policy without adding a different one.

## Validation

`checks.x86_64-linux.static-sites` covers invalid configurations and disablement.
`checks.x86_64-linux.james-guide-acme` exercises real TLS-ALPN issuance, multiple
sites, cache selection, conditional requests, SPA fallback, restricted SSH/rsync,
symlink handling, directory ownership and reboot persistence in a VM.
