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
  pname = "link-cli";
  version = "0.29.0";

  src = fetchFromGitHub {
    owner = "stripe";
    repo = "link-cli";
    tag = "@stripe/link-cli@${finalAttrs.version}";
    hash = "sha256-ueUBqVp9peGK+Qot0sI8dWGhay6Cw0AOycgKR8KM3q8=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-q7RNmboqJ43UlVnn5blu32UcYOSEoRlhcxnwC+IZ0ng=";
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
    # Drop dangling workspace links for packages not included in this CLI-only output.
    find "$out/lib/link-cli" -type l ! -exec test -e {} \; -delete
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

  passthru.updateScript = writeShellScriptBin "update-link-cli" ''
    set -euo pipefail
    cd "$(${lib.getExe git} rev-parse --show-toplevel)"
    export PATH="${
      lib.makeBinPath [
        git
        nix
        nix-update
      ]
    }:$PATH"
    exec ${lib.getExe nix-update} link-cli --flake \
      --override-filename pkgs/link-cli/package.nix \
      --version=stable --use-github-releases \
      --version-regex '@stripe/link-cli@(.*)' \
      --url https://github.com/stripe/link-cli "$@"
  '';

  meta = {
    description = "CLI for agents to use a Link wallet";
    homepage = "https://github.com/stripe/link-cli";
    license = lib.licenses.mit;
    mainProgram = "link-cli";
    platforms = lib.platforms.unix;
  };
})
