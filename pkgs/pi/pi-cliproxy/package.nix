{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-cliproxy";
  version = "0.1.0-unstable-2026-11-11";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "2ae7ad3d73621afc2e766d1fded11ddfcefbcbf6";
    hash = "sha256-U8B5+GNYjMi/TrB77aSkckntU/CoSWR+9bAtWD+k4hc=";
  };
  sourceRoot = "source/pi-cliproxy";

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
