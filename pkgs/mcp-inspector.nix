{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
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
  version = "2.11.0";

  src = fetchFromGitHub {
    owner = "modelcontextprotocol";
    repo = "inspector";
    rev = version;
    hash = "sha256-VDwF7YRXeUX8zrOhruKZ13WMC3+zKNgEjZI0PywLLP8=";
  };

  makeWrapperArgs = [
    "--prefix PATH : ${
      lib.makeBinPath [
        nodejs
      ]
    }"
  ];

  npmDepsHash = "sha256-3E/FDudI4OJXXaqJjdtVAoaPnpPgrSVfB6RXO6eOSuM=";

  doCheck = false;

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
