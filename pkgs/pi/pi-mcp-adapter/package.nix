{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-mcp-adapter";
  version = "2.32.1";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-mcp-adapter";
    rev = "v2.32.1";
    hash = "sha256-/NrC8cVEdhswKEQcuVugNSOCGJ3/c6k2Qg8o6hg0X14=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';
  npmDepsHash = "sha256-xo2upLJNRRZ+DycrnO+d2g8k1rqFd3ZzLpMw/NLI5V8=";
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
