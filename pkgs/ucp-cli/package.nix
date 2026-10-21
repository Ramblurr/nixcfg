{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchPnpmDeps,
  pnpmConfigHook,
  pnpm_10,
  nodejs,
  makeWrapper,
  git,
  nix,
  nix-update,
  writeShellScriptBin,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ucp-cli";
  version = "0.9.1";

  src = fetchFromGitHub {
    owner = "Shopify";
    repo = "ucp-cli";
    tag = "v${finalAttrs.version}";
    hash = "sha256-r3J1QSBUPwOrVQDnFhqGp57T15UeRM/L2CUkYpCdbXo=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-nrXc0UmFyk8McDQvIrLdpvjwLta6XBybRWtVihBBdyc=";
  };

  nativeBuildInputs = [
    nodejs
    pnpm_10
    pnpmConfigHook
    makeWrapper
  ];
  env.npm_config_ignore_scripts = "true";

  buildPhase = ''
    runHook preBuild
    pnpm exec tsup
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    pnpm prune --prod --ignore-scripts
    mkdir -p "$out/lib/ucp-cli"
    cp -r dist node_modules package.json skills "$out/lib/ucp-cli/"
    makeWrapper ${lib.getExe nodejs} "$out/bin/ucp" \
      --add-flags "$out/lib/ucp-cli/dist/bin.js"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/ucp" --version | grep -F '${finalAttrs.version}'
    "$out/bin/ucp" --help > /dev/null
    runHook postInstallCheck
  '';

  passthru.updateScript = writeShellScriptBin "update-ucp-cli" ''
    set -euo pipefail
    cd "$(${lib.getExe git} rev-parse --show-toplevel)"
    export PATH="${
      lib.makeBinPath [
        git
        nix
        nix-update
      ]
    }:$PATH"
    exec ${lib.getExe nix-update} ucp-cli --flake \
      --override-filename pkgs/ucp-cli/package.nix \
      --version=stable --use-github-releases \
      --url https://github.com/Shopify/ucp-cli "$@"
  '';

  meta = {
    description = "Reference CLI and MCP server for the Universal Commerce Protocol";
    homepage = "https://github.com/Shopify/ucp-cli";
    license = lib.licenses.mit;
    mainProgram = "ucp";
    platforms = lib.platforms.unix;
  };
})
