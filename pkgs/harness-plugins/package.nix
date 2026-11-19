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
    rev = "a65483e02eb822680215f885da39581b4594a1b6";
    hash = "sha256-1yrYaBwMdmJm7tXPxWioin/GkZuftYFJEtOoHnD5r0U=";
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
