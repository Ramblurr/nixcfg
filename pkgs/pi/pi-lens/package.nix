{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-lens";
  version = "4.5.2";

  # The npm release includes compiled code and grammars; no prepare script needed.
  src = fetchurl {
    url = "https://registry.npmjs.org/pi-lens/-/pi-lens-4.5.2.tgz";
    hash = "sha256-i08AGBw7mykglT4WS6evq3AllqqFE24wC8w8EkODW5I=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-14n+ySty787oBCVsnKc0ia8Fhm9oIEDqgVSRzVkdHVo=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [
    "--omit=dev"
    "--ignore-scripts"
  ];
  npmRebuildFlags = [ "--ignore-scripts" ];
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    # Expose bundled grammars at the dependency's conventional asset location too.
    ln -s ../../grammars "$out/node_modules/web-tree-sitter/grammars"
    runHook postInstall
  '';

  meta = {
    description = "Language-aware code feedback extension for Pi";
    homepage = "https://github.com/apmantza/pi-lens";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
