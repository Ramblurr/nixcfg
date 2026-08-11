{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-hashline-edit";
  version = "0.8.3-unstable-2026-08-16";

  src = fetchFromGitHub {
    owner = "RimuruW";
    repo = "pi-hashline-edit";
    rev = "850b17fcc825450a1664d4bb6f6a73c2ea455c86";
    hash = "sha256-MCbns/lSlLb8D85CBerUGB8AoQUWhsVXITIs8GK1hmw=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-lbkzit56A/PeQBrfQD0doaBev4fkoTZ3/DM52tn8E/E=";
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [ "--omit=dev" ];
  npmRebuildFlags = [ "--ignore-scripts" ];
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  meta = {
    description = "Hash-anchored read and edit tool override for Pi";
    homepage = "https://github.com/RimuruW/pi-hashline-edit";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
