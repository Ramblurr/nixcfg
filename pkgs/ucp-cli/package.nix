{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchPnpmDeps,
  pnpmConfigHook,
  pnpm_10,
  nodejs,
  makeWrapper,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ucp-cli";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "Shopify";
    repo = "ucp-cli";
    tag = "v${finalAttrs.version}";
    hash = "sha256-X/H2fGqX8eS1tTFsidp4AsWX0IYQAQltjELlM1zqPb0=";
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

  meta = {
    description = "Reference CLI and MCP server for the Universal Commerce Protocol";
    homepage = "https://github.com/Shopify/ucp-cli";
    license = lib.licenses.mit;
    mainProgram = "ucp";
    platforms = lib.platforms.unix;
  };
})
