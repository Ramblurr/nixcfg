{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
}:

buildNpmPackage rec {
  pname = "pi-web";
  version = "1.202606.3";

  src = fetchFromGitHub {
    owner = "jmfederico";
    repo = "pi-web";
    rev = "v${version}";
    hash = "sha256-D8LVjjdOjBAi6S227CBk9ywkvV34NS9x6hyv+TskhRY=";
  };

  postPatch = ''
    # Upstream package-lock has these nested npm registry entries without integrity.
    # npm-config-hook needs integrities to prefetch the dependency closure.
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-agent-core/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.79.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.79.1.tgz",\n      "integrity": "sha512-PBPjBa2YBm9jauiLtHAKaSfVJ4Dvm3/nK/bR/oHebLjwBCS2tGx3aQDX7MSGAOXi6BejlhzbB/z82BkyAyNjjQ==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-ai/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.79.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.79.1.tgz",\n      "integrity": "sha512-UnORwrcsTNLm4StEvoM8iEom0u87Te7BXEWxhec3iNXygWD6eEBosUoq9ddcveqtj/QpUZBMPWUu81cCtZxzkQ==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-tui/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.79.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.79.1.tgz",\n      "integrity": "sha512-YvZCMfSE0YDSLNklAwMY6LC6SyEgnP0zMOoioTLNnXFNdexrCexMJdee7iDJsNcFlKt7+DVLccomuURtZS1C6g==",#' package-lock.json

    # Respect PI_WEB_DATA_DIR for archived-session state instead of using ~/.pi-web.
    substituteInPlace src/server/sessions/sessionArchiveStore.ts \
      --replace-fail 'import { homedir } from "node:os";' 'import { piWebDataDir } from "../../config.js";' \
      --replace-fail 'join(homedir(), ".pi-web", "archived-sessions.json")' 'join(piWebDataDir(), "archived-sessions.json")'
  '';

  npmDepsHash = "sha256-wWmBOaaubi8hGJt+fm1aAlSbY3a3zq/vU1ZCIqrRT/c=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = ''
    runHook preBuild
    npm run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/node_modules/pi-web
    cp -r dist node_modules package.json $out/lib/node_modules/pi-web/

    # Remove any dangling npm workspace/package-manager symlinks before exposing the tree.
    find $out/lib/node_modules/pi-web -type l ! -exec test -e {} \; -delete 2>/dev/null || true

    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/pi-web \
      --add-flags "$out/lib/node_modules/pi-web/dist/cli.js"
    makeWrapper ${nodejs}/bin/node $out/bin/pi-web-server \
      --add-flags "$out/lib/node_modules/pi-web/dist/server/index.js"
    makeWrapper ${nodejs}/bin/node $out/bin/pi-web-sessiond \
      --add-flags "$out/lib/node_modules/pi-web/dist/server/sessiond.js"

    runHook postInstall
  '';

  meta = {
    description = "Web control plane for persistent Pi Coding Agent sessions";
    homepage = "https://pi-web.dev/";
    changelog = "https://github.com/jmfederico/pi-web/releases/tag/v${version}";
    license = lib.licenses.mit;
    mainProgram = "pi-web";
    platforms = lib.platforms.linux;
  };
}
