{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  babashka,
  git,
  nix,
  nix-update,
  pkgs-lib,
}:

stdenvNoCC.mkDerivation {
  pname = "nixbot-cli";
  version = "0-unstable-2026-08-12";

  src = fetchFromGitHub {
    owner = "outskirtslabs";
    repo = "nixbot-cli";
    rev = "f9c1a0622830c90120042dd6c910bee6dab12555";
    hash = "sha256-zUVOhKUmyzlyLd/GfR2+ag8b4hnBzoUF47g6Ag7NTJ8=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 nixbot-cli $out/bin/nixbot-cli
    wrapProgram $out/bin/nixbot-cli \
      --prefix PATH : ${
        lib.makeBinPath [
          babashka
          git
        ]
      }

    runHook postInstall
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "nixbot-cli";
    utils = [
      git
      nix
      nix-update
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Inspect and control Nixbot CI builds from the terminal";
    homepage = "https://github.com/outskirtslabs/nixbot-cli";
    license = lib.licenses.eupl12;
    maintainers = [ lib.maintainers.ramblurr ];
    platforms = babashka.meta.platforms;
    mainProgram = "nixbot-cli";
  };
}
