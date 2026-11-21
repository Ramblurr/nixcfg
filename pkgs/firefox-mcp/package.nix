{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  firefox,
  geckodriver,
  git,
  nix,
  nix-update,
  pkgs-lib,
}:

buildNpmPackage (finalAttrs: {
  pname = "firefox-mcp";
  version = "0.11.0";

  src = fetchFromGitHub {
    owner = "mozilla";
    repo = "firefox-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-ELxWz0N7IfRGeE6WlxwH68bqP2ZRmuPVV0z2W10gQGQ=";
  };

  npmDepsHash = "sha256-DWVdaLsJINQ0EloydHX2KjvY1AfpyEeO5saYiF1Pdvw=";

  env.npm_config_ignore_scripts = "true";

  # On x86-64 Linux upstream skips its PATH lookup for geckodriver and runs
  # Selenium Manager, which downloads geckodriver and Firefox.
  postPatch = ''
    substituteInPlace src/firefox/core.ts --replace-fail \
      "process.platform === 'win32' || (process.platform === 'linux' && process.arch !== 'x64');" \
      "true;"
  '';

  # geckodriver finds Firefox on PATH; the nixpkgs one is a fallback so a PATH Firefox wins.
  makeWrapperArgs = [
    "--prefix PATH : ${lib.makeBinPath [ geckodriver ]}"
    "--suffix PATH : ${lib.makeBinPath [ firefox ]}"
  ];

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    $out/bin/firefox-mcp --version | grep -F ${lib.escapeShellArg finalAttrs.version}
    runHook postInstallCheck
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "firefox-mcp";
    utils = [
      git
      nix
      nix-update
    ];
    script = ./update.bb;
  };

  meta = {
    description = "MCP server that lets AI agents browse the web and debug web pages with Firefox";
    homepage = "https://github.com/mozilla/firefox-mcp";
    changelog = "https://github.com/mozilla/firefox-mcp/blob/v${finalAttrs.version}/CHANGELOG.md";
    license = [
      lib.licenses.mit
      lib.licenses.asl20
    ];
    maintainers = [ lib.maintainers.ramblurr ];
    mainProgram = "firefox-mcp";
  };
})
