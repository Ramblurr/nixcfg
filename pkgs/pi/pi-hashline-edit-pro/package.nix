{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit-pro";
  version = "2.5.3";

  src = fetchFromGitHub {
    owner = "YuGiMob";
    repo = "pi-hashline-edit-pro";
    rev = "1635cbfd9e7ea3d51f262774b08ded1948caa3ba";
    hash = "sha256-3zVXKe9/d37F8ja015AdszhY0soCfhpxrna8emfyN0E=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-Ef4Qt2BRk1JNLMSdnXi/rMl/SN8/4QDRE72BrtvgCsU=";
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
    description = "Hash-anchored read, replace, and undo tools for Pi";
    homepage = "https://github.com/YuGiMob/pi-hashline-edit-pro";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
