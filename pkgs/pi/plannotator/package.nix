{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "plannotator-pi-extension";
  version = "0.28.38";

  src = fetchurl {
    url = "https://registry.npmjs.org/@plannotator/pi-extension/-/pi-extension-0.28.38.tgz";
    hash = "sha256-YDSQ53wkQRqixSUghp00y+74yzKrf5/iX0JkNJZBxZM=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-3VAsB5HqTeJ+793EuhUPXCzajUFIiyjyKwfiqaJW8Pg=";
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
