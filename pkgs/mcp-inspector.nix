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
  version = "2.16.0";

  src = fetchFromGitHub {
    owner = "modelcontextprotocol";
    repo = "inspector";
    rev = version;
    hash = "sha256-FpTzpN9/B4V4ko4SCTByBJHM+XHFM7ZzseDzyc773R4=";
  };

  makeWrapperArgs = [
    "--prefix PATH : ${
      lib.makeBinPath [
        nodejs
      ]
    }"
  ];

  npmDepsHash = "sha256-fC0/13fgpHDlZxZ+FaY0+4VOQfxCFAtLNvJOnErM+lo=";

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
        cli = "sha256-Fbi/6HLq4Q6sSgUeySa66LDHepcA5GdvsR9RIMHOubE=";
        mcpdo = "sha256-ikB1o5YxQ8qO3VTtY3F8cNY9Sycx09L0vttetA2QEkw=";
        tui = "sha256-b7/he7GMzgFsFQ6v2KWIv0CTTyNEy13Rr5Ye6hTE9TI=";
        web = "sha256-k8uI5cSuhEKAMFl+UMPElvu9RB9zbapwp+yd/4PJX7A=";
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
