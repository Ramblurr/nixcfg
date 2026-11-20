{
  lib,
  stdenvNoCC,
  savantConfigured ? false,
}:
stdenvNoCC.mkDerivation {
  pname = "pi-cliproxy";
  version = "0.1.0-unstable-2026-11-20";

  src = (import ./source.nix).outPath;
  sourceRoot = "source/pi-cliproxy";
  patches = [ ./only-discover-advertised-savant.patch ];
  postPatch = lib.optionalString savantConfigured ''
    substituteInPlace index.ts --replace-fail '/* configured-savant */ false' 'true'
  '';

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
