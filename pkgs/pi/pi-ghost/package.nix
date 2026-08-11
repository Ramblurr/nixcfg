{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-ghost";
  version = "0.1.4-unstable-2026-08-11";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "27393860d3b8b7cf031e5e02a0684f7f477e5c04";
    hash = "sha256-oPWEdkZfnxY0uo5dkx55/GWNCTEM8dtzXIFz4SLDZ34=";
  };
  sourceRoot = "source/pi-ghost";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Ghost overlay for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions";
    platforms = lib.platforms.all;
  };
}
