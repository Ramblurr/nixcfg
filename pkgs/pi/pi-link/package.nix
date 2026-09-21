{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "alvivar";
    repo = "pi-link";
    rev = "aff9d5a8f9fa43cc101ef6ee75d239c215e36069";
    hash = "sha256-j+BaegjJ+6Pb0feEaCIkpXketxyAiiWqCcpL6BVu2bU=";
  };

  # Native link_control tool, rebased from Ramblurr/feat/pi-link-control-tool.
  patches = [ ./link-control.patch ];

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-gISETDw+XPG4IiWClJs+pRL0p++qdpvvEixPcMYGMa8=";
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
