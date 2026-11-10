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
  pname = "ol-skills";
  version = "0-unstable-${builtins.substring 0 7 finalAttrs.src.rev}";

  src = fetchFromGitea {
    domain = "git.outskirtslabs.com";
    owner = "casey";
    repo = "skills";
    rev = "ea3cbf23f4115549b34a3cf7baf295482aad9f5f";
    hash = "sha256-PQs8rAG5h2XyoU5gB/XslXJoWRA/CyqXBD1m/ptctWU=";
  };

  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -a skills .claude-plugin "$out/"
    runHook postInstall
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "ol-skills";
    utils = [
      git
      nix
      ast-grep
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Outskirts Labs agent skills and Claude plugin manifests";
    homepage = "https://git.outskirtslabs.com/casey/skills";
    platforms = lib.platforms.all;
  };
})
