{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
}:

let
  rev = "165928a54308233b8bb318d927c5c78496833d4a";
in
buildNpmPackage rec {
  pname = "pi-web";
  version = "1.202607.1-unstable-2026-07-23";

  src = fetchFromGitHub {
    owner = "jmfederico";
    repo = "pi-web";
    inherit rev;
    hash = "sha256-GhrVec9b1GCWnTE7p583SwglkcgJaWapKtN64Ttw2Rw=";
  };

  postPatch = ''
    # Upstream package-lock has these nested npm registry entries without integrity.
    # npm-config-hook needs integrities to prefetch the dependency closure.
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-agent-core/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.81.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.81.1.tgz",\n      "integrity": "sha512-yqbh68CyhqxMov/jUogFJfMqlu2Gd37GAki+tr59YCmAPHfomiCA5ESzusXtpGzABeiZFC/OrRdQ4GwCCOMIHA==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-ai/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.81.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.81.1.tgz",\n      "integrity": "sha512-hzHE7Z8l5mgJk+ke67Lge0rwS2+wbKJrFKl9o5M1R1rh33+cCT7D1AHz1OAtX5wFs90E1/BTGhyJRTUHaMxGvQ==",#' package-lock.json
    sed -i '/node_modules\/\@earendil-works\/pi-coding-agent\/node_modules\/\@earendil-works\/pi-tui/,+2 s#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.81.1.tgz",#"resolved": "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.81.1.tgz",\n      "integrity": "sha512-OMEe+Zt8oQYi/rCq3upxsTlIScWL0FPhXwQus34TbQb3EmTx88S7Uzx32JxvQiEeWOw8eDCdJf2PBUBE9r6wIg==",#' package-lock.json
  '';

  npmDepsHash = "sha256-WuULzZgs56rfA5G9yJ0K/bt7lYlvE7Oo8+99Yb6vmXE=";
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
    changelog = "https://github.com/jmfederico/pi-web/compare/v1.202607.1...${rev}";
    license = lib.licenses.mit;
    mainProgram = "pi-web";
    platforms = lib.platforms.linux;
  };
}
