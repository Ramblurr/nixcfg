{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "pi-subq";
  version = "0.1.0-unstable-2026-11-20";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "d179663f8417122eefb6f0f893dfbef353127349";
    hash = "sha256-v1YKg+4BrMnbprwm1mYlaIrdkXW0TFk/0p7vntWJI/A=";
  };
  sourceRoot = "source/pi-subq";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "SubQ provider extension for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
