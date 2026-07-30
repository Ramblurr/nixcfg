{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
}:

let
  rev = "7103bfcb4c13565a79d248f124fdb3118f778928";
in
buildNpmPackage rec {
  pname = "pi-web";
  version = "1.202607.3-unstable-2026-07-30";

  src = fetchFromGitHub {
    owner = "jmfederico";
    repo = "pi-web";
    inherit rev;
    hash = "sha256-Ya0UUeKtYjZxUqP7hM+U7k+aKSShEGjhNXg/gJpsGX8=";
  };

  postPatch = ''
    # Upstream package-lock has these nested npm registry entries without integrity.
    # npm-config-hook needs integrities to prefetch the dependency closure.
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-agent-core/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.82.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.82.1.tgz",\n      "integrity": "sha512-Z3kloziJIE2dmrisRckZX8zDca/gIv9/YdFAzeoqpHiLV2wsni6bL4hInNSjVKLbqT+4kqLIkph2JQLKvSepjg==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-ai/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.82.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.82.1.tgz",\n      "integrity": "sha512-3WFYRhEp3lQB3444EhPMBcM7zSaEUE3eJgHOR7s4081NLqbw/FsWilIKWXSua0Gv3sRr7m9xMidR3pPDE7jI/A==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-tui/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.82.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.82.1.tgz",\n      "integrity": "sha512-9yN8hALfKaxZq7n54EMxqhFCWnMi6LHkraMJ/1YjHiATq75XrI6XDMVppn9EDtiK7Fks8hUe1SDXUTrIvwRWfQ==",#' package-lock.json
  '';

  npmDepsHash = "sha256-IHTNEXtfh4Q1UcqWCugVC4ybXPA0hcKybKNAqjnoJJ0=";
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
    changelog = "https://github.com/jmfederico/pi-web/compare/v1.202607.3...${rev}";
    license = lib.licenses.mit;
    mainProgram = "pi-web";
    platforms = lib.platforms.linux;
  };
}
