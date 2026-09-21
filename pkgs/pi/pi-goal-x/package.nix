{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-goal-x";
  version = "0.31.6";

  src = fetchFromGitHub {
    owner = "tmonk";
    repo = "pi-goal-x";
    rev = "50f677edc1038acd02c0c1652ad0d0d9694e0553";
    hash = "sha256-zToK1YvBWHfLGmZW0mgdbW17V7o93cR8r/j9id0Orrg=";
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
