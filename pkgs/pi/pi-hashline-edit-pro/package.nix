{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit-pro";
  version = "7.2.1";

  src = fetchFromGitHub {
    owner = "YuGiMob";
    repo = "pi-hashline-edit-pro";
    rev = "e4811ccc565ff79551910d6fd3c0200ea44eacac";
    hash = "sha256-gftfMApuFQypevdObDq+qX4LVIV/e9Q4L5EBqIaof0Q=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-ZvTxc6wnHlZzTRHkUe/8ZjFxqNSmiDtE0sEXBHF7iIg=";
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
