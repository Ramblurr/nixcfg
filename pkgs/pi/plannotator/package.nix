{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "plannotator-pi-extension";
  version = "0.28.49";

  src = fetchurl {
    url = "https://registry.npmjs.org/@plannotator/pi-extension/-/pi-extension-0.28.49.tgz";
    hash = "sha256-DoDWSisfP7v1wTW6VgCRlYMQwePuC6or9id1w0MHPm8=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-HH6jnZSGiDLLt7R8nT/DBw0jlswT5SGnEzJgx9MnKQU=";
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
