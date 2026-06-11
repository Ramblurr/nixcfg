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
  version = "0-unstable-2026-06-11";

  src = fetchFromGitHub {
    owner = "outskirtslabs";
    repo = "nixbot-cli";
    rev = "e6f96dbcca4bf024fa06dc2d43bfc9936e72eeb1";
    hash = "sha256-84865/16hCWzN70kkLKl6MzV623eO8bxCxhrY/H2nRw=";
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
