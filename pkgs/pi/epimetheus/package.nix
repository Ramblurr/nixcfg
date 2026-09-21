{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "epimetheus";
  version = "0.7.0";

  src = fetchFromGitHub {
    owner = "noctuid";
    repo = "epimetheus";
    rev = "2d53c9b439961f1f649284c624c8d82d322bd348";
    hash = "sha256-HhcFA0e8fwCWoBA3e2Ax3t1dfC/pzbZ1am2pnDDQd8g=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-0qtCJeYUqdo3E7HMGhDwMI1HEJlq8gZDKcjcQKr05PU=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [ "--omit=dev" ];
  npmRebuildFlags = [ "--ignore-scripts" ];
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Hindsight AI memory extension for Pi";
    homepage = "https://github.com/noctuid/epimetheus";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.all;
  };
})
