{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-goal-x";
  version = "0.32.3";

  src = fetchFromGitHub {
    owner = "tmonk";
    repo = "pi-goal-x";
    rev = "64c5ace87f3400c0b55ef87f2d2912167f53dbaa";
    hash = "sha256-d0IRFiK5JH/VDimVEdJFE/+5i4lDlPLXITvcbCpixt0=";
  };

  # Runtime dependencies are peers supplied by Pi; TypeScript loads directly.
  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/scripts"
    cp -r extensions docs assets package.json README.md LICENSE "$out"
    cp scripts/recover-session-checkpoints.mjs "$out/scripts"
    runHook postInstall
  '';

  meta = {
    description = "Persistent goals, task planning, and completion auditing for Pi";
    homepage = "https://github.com/tmonk/pi-goal-x";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
