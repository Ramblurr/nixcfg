{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-heartbeat";
  version = "0.1.0-unstable-2026-10-30";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "286a8927686722121b5ff85a1c12e5b4f122834d";
    hash = "sha256-fNNNCz+oTjOsmnBobztsGk9VTd5ojU/AmzGB9zCdK8M=";
  };
  sourceRoot = "source/pi-heartbeat";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Prompt the current Pi agent after a period of continuous idle time";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
