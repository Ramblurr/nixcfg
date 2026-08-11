{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "epimetheus";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "noctuid";
    repo = "epimetheus";
    rev = "b3dacfa21cc7a5e8b6a9080f68fa36540a44bdcf";
    hash = "sha256-vUOTuwaIe6Yyq9Vu7s2lxyDtPeV3UHsng/Yg8uxS+E4=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-QXp+lW5fsjhrAQPSdcOhSZ394SCHpaYiMkQGa1UiDdg=";
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
