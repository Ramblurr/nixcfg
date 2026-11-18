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
    rev = "08bb04ca4d9ff74d42a0e02f9deb5d48f9c46fd0";
    hash = "sha256-By5QFeqH4s7YfoJsusxUkUKKuLH1HG5K/IX8XVuIQ+g=";
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
