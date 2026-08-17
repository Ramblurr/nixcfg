{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-heartbeat";
  version = "0.1.0-unstable-2026-08-17";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "7f01a16f571c37fc6a0e7bbf82878fdf6b5888c0";
    hash = "sha256-zj3jhQvI42C+G4wXyRxuXWiOCYYAnnX8zujUwQPQh/8=";
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
