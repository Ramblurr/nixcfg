---
name: authentik-cli
description: Inspect live Authentik state on dewey with the ak CLI. Use when debugging Authentik apps, proxy providers, outposts, or embedded outpost attachments on dewey; when checking effective Authentik runtime config; or when running read-only Django ORM queries against the live Authentik database. Use this skill instead of guessing the ak path or invoking ak without the service environment.
---

# Authentik CLI on Dewey

Use `ak` from the same environment as `authentik-server`.

## Prefer a transient systemd unit

Run DB-backed `ak` commands as Unix user `authentik`. Do not run them as `root`; PostgreSQL peer auth will fail. Do not invoke a bare `ak` from SSH shell `PATH`; it will miss both the Nix store path and required Authentik environment.

Use `systemd-run` to launch a transient process with the same user, working directory, secret env file, and unit environment as `authentik-server`.

Use this wrapper and replace the final `ak ...` part with the subcommand you need:

```bash
ssh dewey 'sudo systemd-run --quiet --pipe --wait --collect \
  --uid=authentik --gid=authentik \
  --working-directory="$(systemctl show authentik-server -p WorkingDirectory --value)" \
  -p EnvironmentFile=/run/secrets/authentik/env \
  env $(systemctl show authentik-server -p Environment --value) \
  ak dump_config'
```

This works because `systemctl show authentik-server -p Environment --value` includes the Authentik Nix store `PATH`, so `ak` resolves correctly inside the transient unit without hardcoding the store path.

## Use read-only entrypoints

Start with one of these:

- `ak dump_config`
- `ak --help`
- `ak shell -c '...'`

Expect noisy startup logs before the actual command output. That is normal.

## Inspect the embedded outpost

Replace the final `ak dump_config` part in the wrapper with:

```bash
ak shell -c 'from authentik.outposts.models import Outpost; o=Outpost.objects.get(name="authentik Embedded Outpost"); print(o.name); print(sorted(o.providers.values_list("pk", flat=True)))'
```

Use this to verify which proxy providers are attached to the live embedded outpost.

## Check whether filebrowser is attached

Replace the final `ak dump_config` part in the wrapper with:

```bash
ak shell -c 'from authentik.core.models import Application; from authentik.outposts.models import Outpost; app=Application.objects.get(slug="filebrowser"); outpost=Outpost.objects.get(name="authentik Embedded Outpost"); providers=set(outpost.providers.values_list("pk", flat=True)); print(app.provider_id); print(app.provider_id in providers)'
```

Use this when `files.socozy.casa` fails auth and you need to confirm whether the `filebrowser` proxy provider is attached to the embedded outpost.

## Confirm the live service environment

Replace the final `ak dump_config` part in the wrapper with:

```bash
ak dump_config
```

Use this to confirm the effective Authentik config on `dewey`, including the live HTTP listener, PostgreSQL socket path, and other environment-derived settings.
