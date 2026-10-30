{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-link-control";
  version = "0.1.0-unstable-2026-10-30";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "286a8927686722121b5ff85a1c12e5b4f122834d";
    hash = "sha256-fNNNCz+oTjOsmnBobztsGk9VTd5ojU/AmzGB9zCdK8M=";
  };
  sourceRoot = "source/pi-link-control";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Native Pi Link connection controls for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
