{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-link-control";
  version = "0.1.0-unstable-2026-09-09";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "8bc8f21ed15f3ac9870898f7a65c08f54872f77a";
    hash = "sha256-oEXSH1iSjLCfX94rxOVDte8bdVRJEwC1adBcjue0KHo=";
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
