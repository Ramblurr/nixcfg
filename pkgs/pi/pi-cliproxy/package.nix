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
    rev = "e06fb51996698aaf2589242ed541413ebc039527";
    hash = "sha256-fcz4CUUmQcGk1IYBRHNVqOpdJqtf4qynTqOYryNvAOU=";
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
