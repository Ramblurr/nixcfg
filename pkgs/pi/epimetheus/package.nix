{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "epimetheus";
  version = "0.6.1";

  src = fetchFromGitHub {
    owner = "noctuid";
    repo = "epimetheus";
    rev = "51a7730561a7588d74929f9130bff3134dc43bb2";
    hash = "sha256-XSdo0SVWxR1hNnimQ3N4yZr11Hl8JbKFNNpfAV98/PA=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-GCpqNsIJcH3PV37EisfhcQ+8PvHBcLczi9+vbWyf5BM=";
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
