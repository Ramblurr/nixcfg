{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-sexp-edit";
  version = "0.1.0-unstable-2026-10-30";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "286a8927686722121b5ff85a1c12e5b4f122834d";
    hash = "sha256-fNNNCz+oTjOsmnBobztsGk9VTd5ojU/AmzGB9zCdK8M=";
  };
  sourceRoot = "source/pi-sexp-edit";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Structural Clojure editing tools for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
