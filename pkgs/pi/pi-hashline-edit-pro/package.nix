{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit-pro";
  version = "7.1.1";

  src = fetchFromGitHub {
    owner = "YuGiMob";
    repo = "pi-hashline-edit-pro";
    rev = "d7145c8cf23bb439e1a4733c7ed0391432c92bc8";
    hash = "sha256-ruzbTJADWx8OOjwViae9jrBei29StBd7k1snKv9NRjE=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-0rEPl3GK1BW0Ye84aER4wR1pcgJoOamUHPDJyIr9u3c=";
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
