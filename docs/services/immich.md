# Immich home instance

## Layout

- `immich-home` is a Dewey microVM. It runs the API, web application, PostgreSQL 18, and persistent Redis.
- Peirce runs the background worker, CUDA machine learning, and NVENC video transcoding.
- Mali exports `tank2/services/immich-home`. The guest and Peirce mount it at `/var/lib/immich` with UID/GID 3024 and root squashing.
- Quine has no Immich service, mount, credential, proxy, or firewall configuration.
- `photos.<home-domain>` uses Dewey's private Caddy route. It is not exposed through James.

The guest's API role uses `IMMICH_WORKERS_INCLUDE=api`. Peirce uses `IMMICH_WORKERS_INCLUDE=microservices`. Peirce's worker connects to the guest PostgreSQL and Redis services and reads the same media tree.

## Machine-learning boundary

Peirce runs the unauthenticated ML service only on `127.0.0.1:3003`. Its background worker uses that loopback endpoint directly.

The remote API caller uses a UID-restricted Caddy proxy on guest loopback port 3004. That proxy authenticates to Peirce's Caddy endpoint on port 3443 with the API client certificate. Peirce admits only the guest service address and explicitly allowlists the API leaf certificate.

The public configuration contains the non-secret 1Password references:

- `Immich ML CA`
- `Immich ML Peirce`
- `Immich ML API`

Actual certificate, key, and token values remain in 1Password or encrypted SOPS. The CA signing key remains in its operator-restricted vault and is never delivered to a runtime host.

Peirce checks the server and API certificates daily. A successful check reports `Immich ML Certificate Expiry (peirce)` to Gatus. Missing success for 36 hours triggers Pushover. The check stops reporting 30 days before either certificate expires.

## Runtime application secrets

The guest and worker use `/var/lib/immich-secrets/environment` for `DB_PASSWORD` and `REDIS_PASSWORD`. The guest also uses `/var/lib/immich-secrets/redis-password`. Directories are root-owned mode `0700`; files are root-owned mode `0600`.

The guest files live in its persistent Dewey backing storage. Peirce persists its secret directory and Immich cache through impermanence. Never provision files into an unmounted backing directory.

## UI settings

1. External URL: `https://photos.<home-domain>`.
2. Machine Learning URL: `http://127.0.0.1:3004`.
3. Video transcoding: NVENC. Enable hardware decoding only after the runtime smoke test passes.
4. Start expensive queue concurrency at 1.
5. Configure native Immich OIDC for Pocket ID; do not add browser forward authentication.

Settings remain database-managed because `services.immich.settings = null`.

## Operations

Peirce owns all background processing. If Peirce stops, uploads can remain accepted by the API, but metadata, thumbnails, transcoding, and ML queues stop until the worker returns. Redis AOF preserves queued work subject to its normal crash window.

Before planned Peirce maintenance, pause queues in the Immich Jobs UI, wait for active jobs where practical, and stop `immich-server.service` and `immich-machine-learning.service`. Queue pause does not cancel active FFmpeg work.

Quine requires one final cleanup deployment when migrating the worker. After that deployment, do not restore an Immich role on Quine.

## Validation

Run source checks in public `nixcfg` and real builds from `nixcfg-private`:

```sh
nix build --no-link .#checks.x86_64-linux.immich-guest \
  .#checks.x86_64-linux.immich-worker \
  .#checks.x86_64-linux.immich-split \
  .#checks.x86_64-linux.immich-machine-learning \
  .#checks.x86_64-linux.immich-ml-proxy

cd ~/nixcfg-private
nix flake update nixcfg
# Exit and re-enter nix develop after the input update.
build debord peirce quine dewey immich-home
```

Before activation, inspect Quine's deployment diff and reject any unexplained network change. After activation, validate LAN access, the default route, DNS, and external IP connectivity immediately.

Ticket 065-08 owns the authenticated ML cutover and Quine cleanup. Ticket 065-07 performs the consolidated image, video, CUDA, NVENC, queue, outage, and final-health pilot.
