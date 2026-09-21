{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-mcp-adapter";
  version = "2.36.0";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-mcp-adapter";
    rev = "v2.36.0";
    hash = "sha256-PYRDVF5QcZFLJP9z5aYhzgWbuv9yvc0lt9tZKxhHyho=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-qhHgQP4JtLI1yJiGpNWDlUoDqiwbIjVpi+GyAD8A7n4=";
  npmDepsFetcherVersion = 2;
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
    description = "MCP adapter extension for Pi";
    homepage = "https://github.com/nicobailon/pi-mcp-adapter";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
