{
  lib,
  stdenv,
  fetchFromGitHub,
  nodejs_24,
  yarn-berry_4,
  makeWrapper,
  babashka,
  basePath ? "",
}:
let
  nodejs = nodejs_24;
  yarn = yarn-berry_4.override { inherit nodejs; };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "neogrok";
  version = "1.2.6";

  src = fetchFromGitHub {
    owner = "isker";
    repo = "neogrok";
    rev = "be7cf6de7936934f3cacce7ea46c092290beb116";
    hash = "sha256-iqwLOErUMV5aHz2Hgbd23cHXlQd4Uddh55ZE3x1UHlE=";
  };

  # Zoekt's cgit templates append ?id= after URLJoinPath.
  patches = [
    ./cgit-url-templates.patch
    ./base-path.patch
  ];
  NEOGROK_BASE_PATH = basePath;

  missingHashes = ./missing-hashes.json;
  offlineCache = yarn.fetchYarnBerryDeps {
    inherit (finalAttrs) src missingHashes;
    hash = "sha256-jjgNFToq8ZIKEpjNf2fukkxSJafwYd3+SnbwUVxnC2Q=";
  };

  nativeBuildInputs = [
    nodejs
    yarn
    (yarn.yarnBerryConfigHook.override { inherit nodejs; })
    makeWrapper
  ];

  postPatch = ''
    sed -i '/^yarnPath:/d' .yarnrc.yml
  '';

  buildPhase = ''
    runHook preBuild
    yarn build
    runHook postBuild
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    yarn vitest run
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    yarn workspaces focus --all --production
    mkdir -p $out/lib/neogrok $out/bin
    cp -r build node_modules main.js package.json $out/lib/neogrok/
    makeWrapper ${lib.getExe nodejs} $out/bin/neogrok \
      --add-flags $out/lib/neogrok/main.js
    runHook postInstall
  '';

  passthru = {
    yarnFetcher = yarn.yarn-berry-fetcher;
    updateScript = [
      "${lib.getExe babashka}"
      ./update.bb
    ];
  };

  meta = {
    description = "Code search frontend for Zoekt";
    homepage = "https://github.com/isker/neogrok";
    license = lib.licenses.mit;
    mainProgram = "neogrok";
    platforms = lib.platforms.linux;
  };
})
