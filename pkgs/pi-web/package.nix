{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
}:

buildNpmPackage rec {
  pname = "pi-web";
  version = "1.202607.1";

  src = fetchFromGitHub {
    owner = "jmfederico";
    repo = "pi-web";
    rev = "v${version}";
    hash = "sha256-llMtBYOqH92KCrz6lUiJRG0rQgwGZQpeCZ9LU8VFPgY=";
  };

  postPatch = ''
    # Upstream package-lock has these nested npm registry entries without integrity.
    # npm-config-hook needs integrities to prefetch the dependency closure.
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-agent-core/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.80.10.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.80.10.tgz",\n      "integrity": "sha512-nwnOR3SuLYGRFfyQm8ri4Nj5VGVAvAM9GuqQd3u7BUQj0d6hmD2F8w7OHAAjThE3CuySIdM+v8E22QJG6/RfCg==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-ai/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.80.10.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.80.10.tgz",\n      "integrity": "sha512-Moe/H8c87yacDGK9dPbWphZNjVsrb3nTrIHycOQJAkFEnY9PYxOOd74+ny44kATfPU9Dm7aTHefar3pZF+UKUA==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-tui/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.80.10.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.80.10.tgz",\n      "integrity": "sha512-c2JO29PbhKPEQ6fgHQKAl0WhwuFqzWfzspMmP+8B5tpDuP+0mvarRbKKg8gq4b+pQx/QX+6aVS4ko7deoyjQjg==",#' package-lock.json
  '';

  npmDepsHash = "sha256-cwMjId1rUZUjUpVmOWJ5U0JUIRrBKovFSGpxa3c6UvU=";
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
