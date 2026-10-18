{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.5.2-unstable-2026-10-18";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-link";
    rev = "9d54b86e5c0f50b8c1aae6f1e9172fa015effcc1";
    hash = "sha256-yIsahLCotGf+DRhXMin8grCmV3tEt+6CMhRWSOs4qoc=";
  };

  npmDepsHash = "sha256-RlJliSkHPApKMNwO+aQjkqtw1LWyPx1EKEawV33YEqY=";
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
    description = "Inter-terminal communication extension for Pi";
    homepage = "https://github.com/Ramblurr/pi-link";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
