{
  lib,
  stdenvNoCC,
  fetchFromGitea,
  pkgs-lib,
  git,
  nix,
  ast-grep,
  jq,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ol-skills";
  version = "0.0.0-unstable.${builtins.substring 0 7 finalAttrs.src.rev}";
  outputs = [
    "out"
    "codexPlugin"
  ];
  nativeBuildInputs = [ jq ];

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
    mkdir -p "$codexPlugin/plugin/skills" "$codexPlugin/plugin/.codex-plugin" "$codexPlugin/.agents/plugins"
    cp skills/LICENSE* "$codexPlugin/plugin/"
    for group in mine engineering codex; do
      for skill in skills/$group/*/SKILL.md; do
        name=$(basename "$(dirname "$skill")")
        if [ -e "$codexPlugin/plugin/skills/$name" ]; then
          echo "Duplicate skill name: $name" >&2
          exit 1
        fi
        cp -a "$(dirname "$skill")" "$codexPlugin/plugin/skills/$name"
      done
    done
    jq -n --arg version ${lib.escapeShellArg finalAttrs.version} \
      '{name:"ol-skills",version:$version,description:"Outskirts Labs agent skills"}' > "$codexPlugin/plugin/plugin.json"
    jq '. + {skills:"./skills"}' "$codexPlugin/plugin/plugin.json" > "$codexPlugin/plugin/.codex-plugin/plugin.json"
    jq -n '{name:"ol-skills",plugins:[{name:"ol-skills",source:{source:"local",path:"./plugin"},policy:{installation:"AVAILABLE",authentication:"ON_INSTALL"},category:"Productivity"}]}' \
      > "$codexPlugin/.agents/plugins/marketplace.json"
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
    description = "Outskirts Labs skills, Claude plugin, and Codex plugin marketplace";
    homepage = "https://git.outskirtslabs.com/casey/skills";
    platforms = lib.platforms.all;
  };
})
