{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  fetchNpmDeps,
  nodejs,
  git,
  nix,
  nix-update,
  writeShellScriptBin,
  ...
}:
# Source: https://github.com/andoriyu/flakes/blob/501a8d446c40e914a9e1b27f3cd40484807a6848/packages/mcp-inspector/default.nix
# Modified to set PATH to include node (See https://github.com/andoriyu/flakes/pull/258)
buildNpmPackage rec {
  pname = "mcp-inspector";
  version = "2.14.0";

  src = fetchFromGitHub {
    owner = "modelcontextprotocol";
    repo = "inspector";
    rev = version;
    hash = "sha256-iCf/hVWYjgAjfHJKjBXtgA39NL87StgnJjwjAmrukWM=";
  };

  makeWrapperArgs = [
    "--prefix PATH : ${
      lib.makeBinPath [
        nodejs
      ]
    }"
  ];

  npmDepsHash = "sha256-EcmIdl7L+3GM8VOag5r+nGmQgE4sjGffnP2pY8LRWtE=";

  # Each v2 client has its own lockfile; install it without the postinstall cascade.
  # The launcher has no dependencies and uses the root package's build tools.
  # https://github.com/modelcontextprotocol/inspector/blob/2.11.0/scripts/install-clients.mjs
  env.npm_config_ignore_scripts = "true";
  passthru.clientNpmDeps =
    lib.mapAttrs
      (
        client: hash:
        fetchNpmDeps {
          name = "${pname}-${version}-${client}-npm-deps";
          inherit src hash;
          sourceRoot = "${src.name}/clients/${client}";
        }
      )
      {
        cli = "sha256-Q/Fgk+lerQXLobJYpLjOFGVk0Sc61deqSeJQG67MYyU=";
        mcpdo = "sha256-ZSsYxDSQTrLSd37tJ90ZChB+IvtPxYpktimSFBEkXgU=";
        tui = "sha256-DEWTLEbnC0XNuAtEhCmE+ZyvMLlwFfP89+GvwX6irfY=";
        web = "sha256-0AwMjW4RxIhC0ZLpD5oV+DgyFIiZZKu+lpiWLwIRBbs=";
      };
  postConfigure = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (client: deps: ''
      (
        npmRoot=clients/${client}
        npmDeps=${deps}
        npmConfigHook
      )
    '') passthru.clientNpmDeps
  );

  doCheck = false;
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    $out/bin/mcp-inspector --version | grep -Fx ${lib.escapeShellArg version}
    $out/bin/mcp-inspector --cli --help > /dev/null
    $out/bin/mcp-inspector --tui --help > /dev/null
    $out/bin/mcpdo --help > /dev/null
    test -s $out/lib/node_modules/@modelcontextprotocol/inspector/clients/web/dist/index.html
    runHook postInstallCheck
  '';

  passthru.updateScript = writeShellScriptBin "update-mcp-inspector" ''
    set -euo pipefail
    cd "$(${lib.getExe git} rev-parse --show-toplevel)"
    export PATH="${
      lib.makeBinPath [
        git
        nix
        nix-update
      ]
    }:$PATH"
    exec ${lib.getExe nix-update} mcp-inspector --flake \
      --override-filename pkgs/mcp-inspector.nix \
      --version=stable --use-github-releases \
      --subpackage clientNpmDeps.cli --subpackage clientNpmDeps.mcpdo \
      --subpackage clientNpmDeps.tui --subpackage clientNpmDeps.web \
      --url https://github.com/modelcontextprotocol/inspector "$@"
  '';

  meta = with lib; {
    description = "Visual testing tool for MCP servers";
    homepage = "https://github.com/modelcontextprotocol/inspector";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
