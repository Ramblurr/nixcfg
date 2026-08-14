{ pkgs }:

pkgs.runCommand "caddy-security-package-test"
  {
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.gnugrep
    ];
  }
  ''
    caddy version > "$TMPDIR/version"
    caddy build-info > "$TMPDIR/build-info"
    caddy list-modules > "$TMPDIR/modules"

    grep -Eq '^dep[[:space:]]+github\.com/greenpau/caddy-security[[:space:]]+v1\.1\.64([[:space:]]|$)' \
      "$TMPDIR/build-info"
    grep -Fxq security "$TMPDIR/modules"
    grep -Fxq http.handlers.authenticator "$TMPDIR/modules"
    grep -Fxq http.authentication.providers.authorizer "$TMPDIR/modules"
    ! grep -Fqi frankenphp "$TMPDIR/modules"

    touch "$out"
  ''
