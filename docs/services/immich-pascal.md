# Immich processing on Peirce Pascal GPU

Peirce's GTX 1070 Ti runs the Immich background worker, CUDA machine learning, and NVENC video transcoding. Quine has no Immich role.

## Service boundary

`modules/services/immich-machine-learning.nix` owns the standalone ML unit on `127.0.0.1:3003`. It can share the `immich` identity with the background worker when the upstream built-in ML unit is disabled.

`modules/services/immich-worker.nix` runs only `IMMICH_WORKERS_INCLUDE=microservices`. It connects to the guest PostgreSQL and Redis services, mounts the Mali media export, and calls ML over Peirce loopback. It has NVIDIA device access for NVENC but does not start another ML service.

`hosts/peirce/immich.nix` selects the Pascal ML package, NVIDIA R580 driver, worker role, mTLS server proxy, and certificate-expiry heartbeat. The remote API reaches only the authenticated proxy on port 3443. Never expose port 3003.

## Package boundary

- Immich 3.1.0 and Python 3.12.14.
- Proprietary NVIDIA R580, tested with 580.178.04.
- Native CUDA 12.9 libraries and cuDNN 9.10.2.21.
- Upstream ONNX Runtime GPU 1.23.2 cp312 wheel.

`pkgs/immich-machine-learning-pascal.nix` isolates the Python and CUDA overrides. Standard CUDA derivations remain unchanged for binary-cache use. cuDNN 9.11 and later dropped Pascal support; review compatibility before upgrades.

NVENC uses the GPU's dedicated encode hardware. The GTX 1070 Ti supports H.264 and HEVC encoding, but not AV1 encoding. ML and transcoding can run together, subject to shared memory, thermal, and decode capacity.

## Checks

From public `nixcfg`:

```sh
nix build --no-link \
  .#checks.x86_64-linux.immich-machine-learning \
  .#checks.x86_64-linux.immich-worker \
  .#checks.x86_64-linux.immich-split \
  .#checks.x86_64-linux.immich-ml-proxy
```

Real host builds and deployments run from `nixcfg-private` after its public input is updated.

## Hardware validation

The opt-in `immich-pascal-validation` package tests CUDA arithmetic, the pinned Immich models, the running HTTP service, and concurrent H.264/HEVC NVENC with image embeddings.

Build it on Peirce:

```sh
nix build --store ssh-ng://peirce --no-link --print-out-paths .#immich-pascal-validation
```

Run the printed store path as `immich` with a new result directory:

```sh
sudo -u immich /nix/store/PRINTED-PATH/bin/immich-pascal-validate \
  /var/cache/immich/validation-$(date -u +%Y%m%dT%H%M%SZ)
```

September 2026 tests passed all six model paths and concurrent NVENC plus image embeddings. This was functional validation, not a production capacity benchmark. Ticket 065-07 must test a representative production upload, queue recovery, GPU provider selection, and concurrent ML/transcoding after the worker moves to Peirce.
