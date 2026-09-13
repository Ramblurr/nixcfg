{ inputs, pkgs }:
let
  primary = pkgs.caddy-with-security;
  stablePkgs = inputs.nixpkgs-stable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
  stable = (stablePkgs.callPackage ../pkgs/caddy/package.nix { }).overrideAttrs (old: {
    # Force independent vendoring: a shared fixed-output cache can hide drift.
    goModules = old.goModules.overrideAttrs (modules: {
      name = "${modules.name}-stable-check";
    });
  });
  mismatchedVersion = builtins.tryEval (
    (pkgs.callPackage ../pkgs/caddy/package.nix {
      caddy = pkgs.caddy.overrideAttrs (_: {
        version = "0.0.0";
        __intentionallyOverridingVersion = true;
      });
    }).drvPath
  );
in
assert !mismatchedVersion.success;
pkgs.runCommand "caddy-package-check" { } ''
  # Both package builds run the version and plugin installation checks.
  for package in ${primary} ${stable}; do
    test -x "$package/bin/caddy"
    test -f "$package/lib/systemd/system/caddy.service"
    test -f "$package/lib/systemd/system/caddy-api.service"
    test -f "$package/share/man/man8/caddy.8.gz"
    test -f "$package/share/bash-completion/completions/caddy.bash"
  done

  # Neither toolchain may regenerate the checked-in module manifests.
  cmp ${primary.src}/go.mod ${../pkgs/caddy/src/go.mod}
  cmp ${stable.src}/go.mod ${../pkgs/caddy/src/go.mod}
  cmp ${primary.src}/go.sum ${../pkgs/caddy/src/go.sum}
  cmp ${stable.src}/go.sum ${../pkgs/caddy/src/go.sum}
  diff -qr ${primary.goModules} ${stable.goModules}
  touch "$out"
''
