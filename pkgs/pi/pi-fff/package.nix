{
  buildNpmPackage,
  fetchurl,
  autoPatchelfHook,
  stdenv,
  lib,
}:

buildNpmPackage {
  pname = "pi-fff";
  version = "0.11.0";

  src = fetchurl {
    url = "https://registry.npmjs.org/@ff-labs/pi-fff/-/pi-fff-0.11.0.tgz";
    hash = "sha256-0lystSl0qMmb4ez4igulVEWUAqiFB5jMci4IsIjSHeM=";
  };
  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-UJd9CU5LrKdrJsLbYo7JT6fhNO5nT04KUC3QT9VBaw0=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--legacy-peer-deps" ];
  npmInstallFlags = [
    "--omit=dev"
    "--ignore-scripts"
  ];
  npmRebuildFlags = [ "--ignore-scripts" ];
  dontNpmBuild = true;

  # Both libfff_c and the ffi-rs Node addon are shipped as platform packages.
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ stdenv.cc.cc.lib ];

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r . "$out"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    node ${./smoke-test.mjs} "$out"
    runHook postInstallCheck
  '';

  meta = {
    description = "FFF-powered file search and autocomplete extension for Pi";
    homepage = "https://github.com/dmtrKovalenko/fff/tree/main/packages/pi-fff";
    license = lib.licenses.mit;
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
