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
  pname = "spdx-util";
  version = "0-unstable-2026-06-11";

  src = fetchFromGitHub {
    owner = "ramblurr";
    repo = "spdx-util";
    rev = "4ed8a9a71f90ebb6f82ef3a1fe9cb7d8e980ccbf";
    hash = "sha256-QWhgb4vHFLHDC1GMtT9zpTqY/V7VsQtaCCP16At0oH8=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 spdx $out/bin/spdx
    wrapProgram $out/bin/spdx \
      --prefix PATH : ${
        lib.makeBinPath [
          babashka
          git
        ]
      }

    runHook postInstall
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "spdx-util";
    utils = [
      git
      nix
      nix-update
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Opinionated CLI tool for managing SPDX licenses and copyright headers";
    homepage = "https://github.com/ramblurr/spdx-util";
    license = lib.licenses.eupl12;
    maintainers = [ lib.maintainers.ramblurr ];
    platforms = babashka.meta.platforms;
    mainProgram = "spdx";
  };
}
