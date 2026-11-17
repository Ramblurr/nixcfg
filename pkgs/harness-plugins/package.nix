{
  lib,
  stdenvNoCC,
  fetchFromGitea,
  pkgs-lib,
  git,
  nix,
  ast-grep,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "harness-plugins";
  version = "0.0.0-unstable.${builtins.substring 0 7 finalAttrs.src.rev}";

  src = fetchFromGitea {
    domain = "git.outskirtslabs.com";
    owner = "casey";
    repo = "skills";
    rev = "80b9d99ba3e7855c5e2ab19847e92472484d004c";
    hash = "sha256-CP/khGwak5glBDSQUZErJxwQ37pGWWC1bj6r0yEnKQQ=";
  };

  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  # The repository is already laid out as Claude, Codex, and Pi marketplaces.
  installPhase = ''
    runHook preInstall
    cp -a . "$out"
    runHook postInstall
  '';

  passthru = {
    plugins = lib.genAttrs [
      "my"
      "pstack"
      "tstack"
    ] (name: "${finalAttrs.finalPackage}/plugins/${name}");

    updateScript = pkgs-lib.writeUpdateScript {
      packageToUpdate = "harness-plugins";
      utils = [
        git
        nix
        ast-grep
      ];
      script = ./update.bb;
    };
  };

  meta = {
    description = "Outskirts Labs agent skills as Claude Code, Codex, and Pi plugins";
    homepage = "https://git.outskirtslabs.com/casey/skills";
    platforms = lib.platforms.all;
  };
})
