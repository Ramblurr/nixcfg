{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit-pro";
  version = "4.3.6";

  src = fetchFromGitHub {
    owner = "YuGiMob";
    repo = "pi-hashline-edit-pro";
    rev = "2cba0df1ffb4579f86fb3fc666883fea00d3c201";
    hash = "sha256-/lcqlvRWntg1QWuK2Y1guwZPg6gLIXD91FcMXzZmJKk=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-MYCn4kNrurIZMtL/+3PBdRMeXNpANL8El+WKV9mVluM=";
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
