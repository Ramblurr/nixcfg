{
  lib,
  stdenv,
  makeWrapper,
  babashka,
  fetchFromGitHub,
  nix-prefetch-github,
  ast-grep,
  gh,
  pkgs-lib,
  ...
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "brepl";
  version = "2.7.1";

  src = fetchFromGitHub {
    owner = "licht1stein";
    repo = "brepl";
    rev = "v${finalAttrs.version}";
    hash = "sha256-Obv2kSEsgZacY4T3HU1/FqTx4y2dRiCgk9j2tPPd3+o=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp brepl $out/bin/brepl
    chmod +x $out/bin/brepl

    sed -i '1s|#!/usr/bin/env.*|#!/usr/bin/env bb|' $out/bin/brepl

    wrapProgram $out/bin/brepl \
      --prefix PATH : ${lib.makeBinPath [ babashka ]} \
      --set BABASHKA_CLASSPATH ""

    runHook postInstall
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "brepl";
    utils = [
      nix-prefetch-github
      ast-grep
      gh
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Bracket-fixing REPL";
    longDescription = ''
      brepl (Bracket-fixing REPL) enables AI-assisted Clojure development by solving
      the notorious parenthesis problem. It validates syntax using Babashka's built-in
      parser and intelligently fixes bracket errors with parmezan—because AI agents
      shouldn't struggle with Lisp parentheses. Provides automatic syntax validation,
      bracket auto-fix, and REPL synchronization. Also works as a fast nREPL client for
      command-line evaluations, file loading, and scripting workflows.
    '';
    homepage = "https://github.com/licht1stein/brepl";
    changelog = "https://github.com/licht1stein/brepl/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.mpl20;
    maintainers = [ ];
    inherit (babashka.meta) platforms;
    mainProgram = "brepl";
  };
})
