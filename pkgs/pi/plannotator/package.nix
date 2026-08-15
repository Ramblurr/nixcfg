{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "plannotator-pi-extension";
  version = "0.27.3";

  src = fetchurl {
    url = "https://registry.npmjs.org/@plannotator/pi-extension/-/pi-extension-0.27.3.tgz";
    hash = "sha256-FPvuuWtePwE1Krdz8djVe/4O9bTd+ab7nlj0JKEv6yQ=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-hG7QZYn1kkk+IVWeZO/HqH1H8q4sAqajyfahM/890Ag=";
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
