{
  buildNpmPackage,
  fetchurl,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-lens";
  version = "4.2.1";

  # The npm release includes compiled code and grammars; no prepare script needed.
  src = fetchurl {
    url = "https://registry.npmjs.org/pi-lens/-/pi-lens-4.2.1.tgz";
    hash = "sha256-Bn/h48DLZMuGtl3KgkQO5u6QhE65wj5pE4XdtQ63BdY=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-gEJIWhDP3Ijs1b9IKJufGgGrlxASp8SbIgJyixryacQ=";
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
