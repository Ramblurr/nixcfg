{ inputs, pkgs }:
let
  primary = pkgs.caddy-with-security;
  # Stable hosts get the same Caddy as unstable hosts: the overlay builds it
  # from the unstable channel, so one go.mod and vendorHash serve every host.
  stablePkgs =
    inputs.nixpkgs-stable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.appendOverlays
      (import ../pkgs/default.nix inputs);
  mismatchedVersion =
    builtins.tryEval
      (pkgs.callPackage ../pkgs/caddy/package.nix {
        caddy = pkgs.caddy.overrideAttrs (_: {
          version = "0.0.0";
          __intentionallyOverridingVersion = true;
        });
      }).drvPath;
in
assert !mismatchedVersion.success;
assert stablePkgs.caddy-with-security.drvPath == primary.drvPath;
pkgs.runCommand "caddy-package-check" { } ''
  # The package build runs the version and plugin installation checks.
  test -x "${primary}/bin/caddy"
  test -f "${primary}/lib/systemd/system/caddy.service"
  test -f "${primary}/lib/systemd/system/caddy-api.service"
  test -f "${primary}/share/man/man8/caddy.8.gz"
  test -f "${primary}/share/bash-completion/completions/caddy.bash"

  # The toolchain may not regenerate the checked-in module manifests.
  cmp ${primary.src}/go.mod ${../pkgs/caddy/src/go.mod}
  cmp ${primary.src}/go.sum ${../pkgs/caddy/src/go.sum}
  touch "$out"
''
