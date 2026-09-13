{
  lib,
  caddy,
}:
let
  moduleVersion = builtins.match ".*github.com/caddyserver/caddy/v2 v([^[:space:]]+).*" (
    builtins.readFile ./src/go.mod
  );
in
assert lib.assertMsg (
  moduleVersion != null && builtins.head moduleVersion == caddy.version
) "caddy-with-security: update src/go.mod and vendorHash for nixpkgs Caddy ${caddy.version}";
caddy.overrideAttrs (_: {
  # Keep module generation out of builds: Go patch upgrades must not rewrite go.mod.
  src = ./src;
  vendorHash = "sha256-6sUI1D92I1KSvzgi+kzkuvNIooJ4QThuC8tt7rOaIsI=";
  subPackages = [ "." ];

  # Retain nixpkgs's packaging, version hook and build tags; verify our plugins too.
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck

    $out/bin/caddy build-info > build-info
    for module in github.com/caddyserver/caddy/v2 github.com/caddy-dns/desec github.com/greenpau/caddy-security; do
      version=$(awk -v module="$module" '$1 == module { print $2 }' ${./src/go.mod})
      test -n "$version"
      awk -v module="$module" -v version="$version" '
        $1 == "dep" && $2 == module && $3 == version { found = 1 }
        END { exit !found }
      ' build-info || { echo "Missing expected dependency: $module $version" >&2; exit 1; }
    done

    $out/bin/caddy list-modules > modules
    for module in dns.providers.desec security http.handlers.authenticator http.authentication.providers.authorizer; do
      grep -Fx "$module" modules
    done

    runHook postInstallCheck
  '';
})
