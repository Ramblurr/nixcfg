{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-lens";
  version = "4.4.3";

  # The npm release includes compiled code and grammars; no prepare script needed.
  src = fetchurl {
    url = "https://registry.npmjs.org/pi-lens/-/pi-lens-4.4.3.tgz";
    hash = "sha256-lMCJhiqQ64nhH677PXeYxxDqztAnm4wnhDrGq8XNy8s=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-6qitPjEElrinXqCe5sIHpOsNkJD2ON2kof6l4EUkuMU=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [ "--omit=dev" ];
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
