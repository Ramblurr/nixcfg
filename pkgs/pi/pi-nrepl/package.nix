{
  lib,
  fetchFromGitHub,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation (_finalAttrs: {
  pname = "pi-nrepl";
  version = "d173f0427bb64d54160f12d5e0aad67403bf1f05";

  src = fetchFromGitHub {
    owner = "ramblurr";
    repo = "pi-nrepl";
    rev = "d173f0427bb64d54160f12d5e0aad67403bf1f05";
    hash = "sha256-RoLC0blH0JRF1bWUQa0zHc/gnUoNBlvSkFx6f9ufIBY=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r package.json dist "$out"
    runHook postInstall
  '';

  meta = {
    description = "Pi nREPL debugging extension";
    homepage = "https://github.com/ramblurr/pi-nrepl";
    license = lib.licenses.eupl12;
    platforms = lib.platforms.all;
  };
})
