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
    rev = "67ac9028391c3bb2bb726a662976a63774b5a051";
    hash = "sha256-oWt1zuaVI0qq4BNjADOeZijULizOjgNcI3Bur8McocQ=";
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
      "my-cc"
      "my-cx"
      "my-pi"
      "my-t3"
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
