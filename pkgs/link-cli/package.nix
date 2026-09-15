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
  pname = "link-cli";
  version = "0.19.1";

  src = fetchFromGitHub {
    owner = "stripe";
    repo = "link-cli";
    tag = "@stripe/link-cli@${finalAttrs.version}";
    hash = "sha256-Hu6aG3DD7cPXljNgkmT5m0C+7cJ8YTLZFlMIBgspt+c=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-yrAZ6W+oTIdtZTs2VHqgqLapDVwcE9osCVOYME6FEYc=";
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
    pnpm --filter @stripe/link-sdk exec tsup
    pnpm --dir packages/cli exec tsup
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    CI=true pnpm install --offline --prod --frozen-lockfile --ignore-scripts
    mkdir -p "$out/lib/link-cli/packages/cli"
    cp -r node_modules "$out/lib/link-cli/"
    cp -r packages/cli/{dist,node_modules,package.json} "$out/lib/link-cli/packages/cli/"
    # The SDK is bundled; pnpm retains these unused workspace links after pruning.
    rm "$out/lib/link-cli/node_modules/.pnpm/node_modules/@stripe/"{link-sdk,link-typescript-config}
    cp README.md LICENSE "$out/lib/link-cli/"
    makeWrapper ${lib.getExe nodejs} "$out/bin/link-cli" \
      --add-flags "$out/lib/link-cli/packages/cli/dist/cli.js" \
      --set NO_UPDATE_NOTIFIER 1
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/link-cli" --version | grep -F '${finalAttrs.version}'
    "$out/bin/link-cli" --help > /dev/null
    runHook postInstallCheck
  '';

  meta = {
    description = "CLI for agents to use a Link wallet";
    homepage = "https://github.com/stripe/link-cli";
    license = lib.licenses.mit;
    mainProgram = "link-cli";
    platforms = lib.platforms.unix;
  };
})
