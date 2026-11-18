{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-cliproxy";
  version = "0.1.0-unstable-2026-11-16";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "0635dd54b741c30a992fd7c1103054fe57c669bb";
    hash = "sha256-MI0V0S4FC+HGqAzKJ/0lbBt/m4cwW1gETVUHLcLWJlk=";
  };
  sourceRoot = "source/pi-cliproxy";
  patches = [ ./keep-static-models.patch ];

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "CLIProxyAPI model provider extension for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
