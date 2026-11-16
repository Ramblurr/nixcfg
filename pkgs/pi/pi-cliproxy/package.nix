{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-cliproxy";
  version = "0.1.0-unstable-2026-11-16";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "54450e4710bfd21b3022a9b1149c72f4a2684e3a";
    hash = "sha256-hvgt0e6P+jcLAnmFFN6gX+hZ5kwyKdk/QzbJqEIHs3o=";
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
