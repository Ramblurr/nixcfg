{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "plannotator-pi-extension";
  version = "0.27.12";

  src = fetchurl {
    url = "https://registry.npmjs.org/@plannotator/pi-extension/-/pi-extension-0.27.12.tgz";
    hash = "sha256-0hblPEEV/YBn32CiyIGJ394zT/LBNesm+KOQLZs5Ix0=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-k9mw7nLWEq2a2oS0SQiC/3oxYTFSpZggOl79dE4605Y=";
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
    description = "Interactive plan review extension for Pi";
    homepage = "https://github.com/backnotprop/plannotator";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
