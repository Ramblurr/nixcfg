{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-matrix-relay";
  version = "0.0.1-unstable-2026-08-11";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-matrix-relay";
    rev = "ef245befca2f7384711aedb36ca3f8ec234bb9da";
    hash = "sha256-0PggWa8btp7NI7niOu8iPqNIzwPQS+X8pVSF+fsL1V4=";
  };
  sourceRoot = "source/extension";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Matrix relay extension for Pi";
    homepage = "https://github.com/Ramblurr/pi-matrix-relay";
    platforms = lib.platforms.all;
  };
}
