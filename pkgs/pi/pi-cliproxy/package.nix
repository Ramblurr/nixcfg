{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-cliproxy";
  version = "0.1.0-unstable-2026-11-19";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "48710f8f0684fd397ad7109c29a483a73b6a08b4";
    hash = "sha256-HOHGAIwV4jFqtx1xFFIH/ie3Z0uZwViunp5l6jNfs3I=";
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
