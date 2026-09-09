{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit-pro";
  version = "4.2.0";

  src = fetchFromGitHub {
    owner = "YuGiMob";
    repo = "pi-hashline-edit-pro";
    rev = "b514ace0e92371f331a1d42fc2f5d5e9c04c348f";
    hash = "sha256-fSYhvoAMNx6QehbNb9LGZ5q0aVNfWDsNfZriEvjWNyw=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-CEjfioZJwUgRPbHXjeoH3pk2KqSccIOm9nc8hYhXt8k=";
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
