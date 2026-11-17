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
    rev = "5f754c406879006ed0c208dd822e0efac8577f2a";
    hash = "sha256-B1xEtFK8Y/m6doWIpJSJwAFl9YgixWRzJt1Qd0BEbN4=";
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
