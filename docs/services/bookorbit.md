# BookOrbit

Dewey enables `home-ops.apps.bookorbit.enable`. The implementation lives in
`modules/services/bookorbit.nix`, using the NixOS module and BookOrbit package
from `nixpkgs-mine`.

- Canonical URL: `https://books.${domain.home}`; public TLS passes through James
  to Dewey. The old `books2` host redirects to `books`, preserving paths.
- Backend: `127.0.0.1:3082`; application authentication, not proxy authentication.
- State: `/var/lib/bookorbit`, on `rpool/encrypted/safe/svc/bookorbit`.
- Database: local PostgreSQL database/user `bookorbit`, using Unix-socket peer
  authentication.
- Library: `/mnt/mali/tank2/media/books`, writable inside the service through
  its supplementary `media` group and explicit `ReadWritePaths`. `UMask=0002`
  keeps new files group-writable. No recursive ownership or mode changes are
  needed for the existing shared library.

## Calibre-Web retirement

Calibre-Web is disabled on Dewey. Its dataset
`rpool/encrypted/safe/svc/calibre-web` remains mounted at `/var/lib/calibre-web`,
including `app.db`. The Calibre library and its `metadata.db` remain intact.
The Calibre container and KOReader Sync are also disabled. Their datasets stay
mounted at `/var/lib/calibre` and `/var/lib/private/koreader-syncd`; no state or
sync database is deleted. Retired services' ingress routes and Gatus checks are
removed. BookOrbit's Gatus check follows the canonical `books` URL.

Filesystem write access does not enable BookOrbit's automatic metadata
write-back or renaming settings. The existing library retains
`fileWriteEnabled=false` and `fileRenameEnabled=false` until separately changed.
A `to-delete` tag is only a tag; the cutover does not delete any books.

## Authentication and secrets

Pocket ID client `bookorbit` is public with S256 PKCE and restricted to the
Admins and Books groups. Use issuer `https://id.${domain.home}`, scopes
`openid profile email groups`, and callback
`https://books.${domain.home}/oauth2-callback`. No client secret is required.
The Pocket ID launch URL must use the same canonical origin.

Local password login remains disabled through
`modules.services.bookorbit.disableLocalAuth`. The administrator is explicitly
linked to Pocket ID. New OIDC users do not receive default elevated permissions;
automatic merging into existing local accounts remains disabled.

The systemd credential provider supplies these existing 1Password fields:

- `op://home-ops-prod/bookorbit/JWT_SECRET`
- `op://home-ops-prod/bookorbit/SETUP_BOOTSTRAP_TOKEN`
- `op://home-ops-prod/bookorbit/BOOK_REQUEST_ENCRYPTION_KEY`

For initial installation, the first two are separate random secrets of at least
32 characters. The encryption key is exactly 64 hexadecimal characters and must
remain stable: changing it makes saved Requests credentials unreadable.
Never put the values in Nix configuration or the store.

The private repository's `bookorbit-home` Swamp definition manages automation
access. Its vault session is origin-bound: changing from `books2` to `books`
requires a fresh isolated human OIDC login, not copying or rebinding old tokens.
See that definition's local runbook for login and recovery procedures.

## Verification

From `~/nixcfg-private`, evaluate the working public tree without changing the lock:

```sh
nix eval --impure --json .#nixosConfigurations.dewey.config \
  --override-input nixcfg git+file:///home/ramblurr/nixcfg \
  --no-write-lock-file \
  --apply 'import /home/ramblurr/nixcfg/tests/bookorbit.nix'
```

Use the private repository's `host-dewey` Swamp model for approved builds and
deployments. After deployment, check BookOrbit, PostgreSQL and Caddy; Calibre-Web,
the Calibre container and KOReader Sync must be inactive. Verify HTTPS health at
`https://books.${domain.home}/api/v1/health`, the old-host redirect, OIDC login,
retained library/book counts, preserved Calibre data, and write permission in
BookOrbit's actual service mount namespace. Gatus monitors the canonical URL.
