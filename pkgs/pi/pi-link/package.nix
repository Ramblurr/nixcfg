{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.5.1-unstable-2026-09-23";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-link";
    rev = "62df3f2571024b462c13dc1a5c4f1d7f7ddeca80";
    hash = "sha256-DsqBLvFW7IWPgQej8dkVvazpIfmaJjLaCdkTjMdEtes=";
  };

  npmDepsHash = "sha256-/HmWipqHnaIMBPdgLX44xPrlkFaNeUNYOHJ2Z8rvMmA=";
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
