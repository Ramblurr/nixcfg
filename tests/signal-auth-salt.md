# Signal 8.26.0 credential-salt reproduction

## Evidence

The Work instance recognized call links but failed to start the lobby with
`getCheckedCallLinkAuthCredentials: no credentials found`. Its preceding
credential-refresh retries failed in `receiveAuthCredentialWithoutPni` with
`TypeError: failed to downcast any to Uint8Array`.

Signal Desktop v8.26.0 (40d2f6791):

- `ts/textsecure/AccountManager.preload.ts` stores `authCredentialSalt` as bytes.
- `ts/sql/Client.preload.ts` omits it from `ITEM_SPECS`, so neither the write nor
  read path converts the value.
- `ts/sql/Server.node.ts` stores items through the JSON functions in
  `ts/sql/util.std.ts`. A Uint8Array becomes an object with numeric keys.
- `ts/services/groupCredentialFetcher.preload.ts` passes that stored salt directly
  to libsignal. Failure processing group credentials prevents the subsequent
  call-link credentials from being processed.
- The package and upstream both specify libsignal 0.100.0. Its
  `node/ts/zkgroup/auth/ClientZkAuthOperations.ts` passes the salt to the native
  function as a Uint8Array.

`signal-auth-salt.cjs` generates synthetic server parameters and credentials with
**the installed native libsignal binary**. It proves the original typed salt
works, the JSON-loaded object throws the exact logged exception, and restoring
those bytes yields an identical valid credential. It applies the actual candidate
patch to a disposable source copy and tests its helper with valid and malformed
values, including repeated reloads. No native calls are mocked.

This is a native-boundary reproduction, not an end-to-end Signal UI test. The
JSON round-trip models the inspected SQL persistence functions; it does not run
Signal's complete SQL/IPC stack. We have not inspected the real account's salt,
so its stored type remains inferred from logs and this matching source path.

## Run

Requirements: Node 24 with `stripTypeScriptTypes`, `patch`, a local upstream
Signal-Desktop checkout at v8.26.0, and the installed libsignal native binary.
No dependency installation, network requests, or profile access are needed.

From `~/nixcfg`:

```sh
node tests/signal-auth-salt.cjs \
  /nix/store/43c5xki30fk3n2w9cn6wiz258ca1vbcn-signal-desktop-8.26.0/share/signal-desktop/app.asar.unpacked/node_modules/@signalapp/libsignal-client/prebuilds/linux-x64/@signalapp+libsignal-client.node \
  ~/src/github.com/signalapp/Signal-Desktop
```

The expected broken case is asserted with `assert.throws`; exit zero means both
reproduction and repair assertions passed. The helper code is extracted from the
patched source and type-stripped; the test does not load Signal or its profile.

## Candidate patch and limits

`patches/signal-auth-salt.patch` restores exactly 16 integer bytes from the legacy
object at the failing consumer. The size comes from `protos/DeviceMessages.proto`.
It preserves already-typed salts and rejects missing keys, extra keys, incorrect
lengths, non-numeric values, fractions, and values outside 0..255.

This is a temporary compatibility patch, not a database migration. It deliberately
leaves persisted state untouched and handles subsequent reloads again. The durable
upstream fix should add the storage mapping and migrate legacy objects together;
adding the mapping alone would feed old objects into the base64 decoder.

The local overlay applies the patch to the existing nixpkgs-mine package. Neither
the fork nor its input pin was changed.

## Build result (2026-09-17)

Package-only build from the private wrapper succeeded:

```sh
nix build --impure --no-link --print-out-paths --expr '
  let
    flake = builtins.getFlake "/home/ramblurr/nixcfg-private";
    pkgs = flake.nixosConfigurations.quine.pkgs;
    overlay = import /home/ramblurr/nixcfg/overlays/nixpkgs-mine-packages.nix
      flake.inputs.nixcfg.inputs;
  in (overlay pkgs pkgs).signal-desktop
'
```

Output: `/nix/store/0iy46hf0m78ac4a5jrv9q7sz4cwhav2n-signal-desktop-8.26.0`.

Not deployed or launched. Live validation still requires approval to deploy,
then restarting Work Signal and checking that credential refresh and call-link
lobby opening succeed, including after another restart. Do not infer call success
from the package build or synthetic test alone.
