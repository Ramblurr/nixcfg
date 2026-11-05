{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage {
  pname = "pi-speak";
  version = "0.0.1-unstable-2026-11-05";

  src = fetchFromGitHub {
    owner = "Ramblurr";
    repo = "pi-extensions";
    rev = "913c45f9a14e8b8ff4c80bfbb5fbfdd9223f4b89";
    hash = "sha256-3LeGyaUoUyaAH0s/fKU3gnsBKCu5TFDQLxv3Nd5yTbk=";
  };
  sourceRoot = "source/pi-speak";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-xbWgfMZjeM6BE1ixq+corM7FS8uydbqtdgqX+WxxND0=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [ "--omit=dev" ];
  npmRebuildFlags = [ "--ignore-scripts" ];
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r src package.json README.md LICENSE node_modules "$out"
    runHook postInstall
  '';

  meta = {
    description = "Local Kokoro and ElevenLabs speech extension for Pi";
    homepage = "https://github.com/Ramblurr/pi-extensions/tree/main/pi-speak";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
