{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-link";
  version = "0.6.1-unstable-2026-11-05";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-link";
    # Ramblurr/pi-link: feat/pi-link-control-tool
    rev = "95af11670440058979212d9da0ef1844bf3e30d8";
    hash = "sha256-+/7mn+Ih/xoMgm7yimnoxTPOD9E5h4T00T1NkIHjXEY=";
  };

  npmDepsHash = "sha256-Wfy7x0dKh2ES0igKSuFkv46REefnuDLTo9z7VpwT+Rc=";
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
