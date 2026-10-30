{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "pi-goal-x";
  version = "0.34.1";

  src = fetchFromGitHub {
    owner = "tmonk";
    repo = "pi-goal-x";
    rev = "b0fc15ebb658a1e9ea60ae65496717a780080f37";
    hash = "sha256-dU4Dk/Q+FroU9WDQbhqRv7qw+pwQRouZX6XM6efxT9o=";
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
