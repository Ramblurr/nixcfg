{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.5.2-unstable-2026-10-30";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-link";
    rev = "91cdc75b36368f7150e9ac45feafb2387358d628";
    hash = "sha256-cnTfrG307bdlC465E8SMghMkYlCkTbG0W8hxFNUP+CM=";
  };

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
