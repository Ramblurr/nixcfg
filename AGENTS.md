# AGENTS.md

## Repository overview

This is a public NixOS configuration repository
It manages multiple systems with Nix flakes and a modular host/module layout.
The repo contains the shared source of truth for host definitions, modules, overlays, packages, and most configuration logic.

There are many untracked files in this repository.
Never add them.
Never delete them.

## Public/private repo split

`~/nixcfg` is the public source repo.
`~/nixcfg-private` is the private wrapper used for real host builds and deploys.

- `~/nixcfg` contains shared Nix code plus SOPS-encrypted files that are safe to keep public.
- `~/nixcfg-private` contains evaluation-time private `.nix` data such as shared secret files and per-host `hosts/<host>/secrets/local.nix`.
- `~/nixcfg-private` imports this repo as a flake input and constructs the real secret-bearing `nixosConfigurations` by overriding `repo.secretFiles.*` and `node.secretsDir`.
- The agent will usually run from `~/nixcfg`, but `nix build`, `nixos-rebuild`, and similar real host commands should be run from `~/nixcfg-private`.
- If a change depends on `config.repo.secrets.*`, assume those values come from the private wrapper repo at evaluation time.
- Do not move evaluation-time secret `.nix` files or plaintext secret values back into this repo.

## Code style guidelines

- Never use `with lib;` or `with pkgs;`.  Older code uses those patterns in places, but new code should not.
- Never use emoji in code, comments, commit messages, or documentation.
- Prefer explicit `lib.` and `pkgs.` qualification.

## Common workflow notes

- Treat this repo as the library/source repo.
- When changing behavior that affects host construction, secret wiring, or deployment outputs, consider whether verification also needs to happen in `~/nixcfg-private`.
- Do not assume that a successful evaluation in `~/nixcfg` means deploys are correct.

## CI setup

- `~/nixcfg/.github/workflows/dispatch.yml` runs on pushes to `main` and triggers `~/nixcfg-private/.github/workflows/update-from-public.yml` via `workflow_dispatch`.
- The private workflow is the real CI entrypoint for secret-bearing updates.
- It rewrites `inputs.nixcfg.url` to the GitHub form, while leaving the local `git+file` version commented out for local use.
- It runs `nix flake update`, stages `flake.nix` and `flake.lock`, and commits with `update flake` when needed.
- It also configures Garnix cache authentication for Determinate Nix using a netrc file built from the `GARNIX_NETRC_PASSWORD` GitHub secret.

## Architecture and structure

### Host system organization

The repository manages several types of systems:

1. Physical hosts in `/hosts/`
   - Each subdirectory is a NixOS host configuration.
   - Hosts may use stable or unstable nixpkgs.
   - Host configs include hardware settings, networking, services, and host-specific modules.

2. Guest VMs in `/guests/`
   - Each subdirectory is a guest or microvm-style configuration.
   - Guests are deployed through the host-side microvm configuration.

### Module system

Reusable NixOS modules live under `/modules/`.
Major areas include:

- desktop
- development tooling
- services
- shell and CLI environment
- hardware
- networking
- users
- security

### Key technologies and patterns

1. Flakes
   - All configurations use flakes.

2. Channels
   - The repo supports both stable and unstable nixpkgs, depending on host.

3. Secrets
   - SOPS-encrypted files such as `*.sops.yaml` and `*.sops.yml` stay in this public repo.
   - Evaluation-time secret `.nix` files live in `~/nixcfg-private` and supply `config.repo.secrets.*`.
   - Plaintext secret values from SOPS should not appear in `/nix/store`.
   - Evaluation-time `.nix` secret data may end up in `/nix/store`, which is why that data lives only in the private repo.

4. Impermanence
   - Some hosts use impermanent root filesystems.

5. Overlays
   - Custom overlays live in `/overlays/`.
   - This includes selective imports from `nixpkgs-mine`.

6. Home Manager
   - User environment management is integrated into host configs.

### Package management

- Custom packages are defined in `/pkgs/`.
- Overlays in `/overlays/` modify or extend package sets.
- Packages are typically referenced as `pkgs.<name>`.

## Important files

- `flake.nix`
  - Main public flake definition.

- `flake/hosts.nix`
  - Declares host inventory and exports reusable host builder helpers through `lib.nixcfg`.

- `flake/nixos.nix`
  - Core host builder logic.

- `modules/default.nix`
  - Module import organization.

- `config/secrets.nix`
  - Default wiring for `repo.secretFiles.*` and `config.repo.secrets.*`.

- `config/site.nix`
  - Site-level data and default site secret wiring.

- `~/nixcfg-private/flake.nix`
  - Private wrapper flake that injects secret file paths and exposes the real deployable host configurations.

## Expectations for agents

- Prefer changes that keep the public/private boundary intact.
- Do not reintroduce hardcoded assumptions that evaluation-time secret files live inside this public repo.
- If you update secret-path plumbing, preserve the wrapper model used by `~/nixcfg-private`.
- Be careful with git status because this repo often contains unrelated untracked files.
- If a task involves deploys, secret-bearing builds, or final host evaluation, note whether the work belongs in `~/nixcfg`, `~/nixcfg-private`, or both, and run real host commands from `~/nixcfg-private`.
- It's ok to flip the flake input in nixcfg-private to the local git+file when doing local development. Flip it back to github when doing a commit + push
- When building/deploying nixos configurations use `build <hostname>` and `deploy <hostname>` these are special wrapper scripts. Don't deviate from this unless you reall need to.
- Only the human operator runs OpenTofu/Terraform commands; the agent must not execute them.
