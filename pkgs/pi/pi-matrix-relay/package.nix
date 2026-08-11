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
    rev = "27beeabc347ae51b225c24f4bba026a1a977c000";
    hash = "sha256-ef8UASsp3Dh3AyIx3wd9w74y3dGBkBTysvrWQTaDTa8=";
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
