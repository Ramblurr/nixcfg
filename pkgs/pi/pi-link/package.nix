{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.2.0-unstable-2026-08-04";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-link";
    rev = "f873410575a8e2af5618f8e4adb93eac1dc67820";
    hash = "sha256-VLBzjbseKnRruAgxeaF1eEaHcf8MekDrM4GPsySMwRU=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-CF2NuBJRUpgsIGnvDKMDycwxD7jwrug4g43HbS+mLoE=";
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
