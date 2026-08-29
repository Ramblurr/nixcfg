{
  lib,
  stdenv,
  runCommand,
  unzip,
  fetchFromGitHub,
  makeWrapper,
  playwright-driver,
  clojure,
  graalvmPackages,
  clojureNixLocker,
  pkgs,
}:
let
  version = "0.9.33-unstable-2026-08-27";
  rev = "bd40aee84dc4c4037c1f348eb826a11d2d033f9f";
  playwrightVersion = "1.61.0";
  driverPlatform =
    if stdenv.hostPlatform.isLinux then
      if stdenv.hostPlatform.isAarch64 then "linux-arm64" else "linux"
    else
      "mac-arm64";
  graalvm = graalvmPackages.graalvm-ce;
  clojureWithGraal = clojure.override { jdk = graalvm; };
  upstreamSrc = fetchFromGitHub {
    owner = "Blockether";
    repo = "spel";
    inherit rev;
    hash = "sha256-X4D3jVxM7WyN5eP4405Y7/K9jJRF8cGVJ4VKeKc1yu8=";
  };
  src = runCommand "spel-${version}-source" { } ''
    cp -r ${upstreamSrc}/. $out
    chmod -R u+w $out
    cp ${./deps.lock.json} $out/deps.lock.json
  '';
  lockerPkgs = pkgs // {
    clojure = clojureWithGraal;
  };
  locked = (import "${clojureNixLocker}/default.nix" { pkgs = lockerPkgs; }).lockfile {
    inherit src;
    lockfile = "deps.lock.json";
  };
in
stdenv.mkDerivation {
  pname = "spel";
  inherit version src;

  nativeBuildInputs = [
    makeWrapper
    unzip
    clojureWithGraal
    graalvm
  ];

  buildPhase = ''
    runHook preBuild

    source ${locked.shellEnv}
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    clojure -T:build native-image

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    driverDir=$out/share/spel/driver/${driverPlatform}
    mkdir -p "$driverDir" driver-unpack
    unzip -q ${locked.homeDirectory}/.m2/repository/com/microsoft/playwright/driver/${playwrightVersion}/driver-${playwrightVersion}.jar -d driver-unpack
    cp -r driver-unpack/driver/. "$driverDir/"
    unzip -p ${locked.homeDirectory}/.m2/repository/com/microsoft/playwright/driver-bundle/${playwrightVersion}/driver-bundle-${playwrightVersion}.jar \
      driver/${driverPlatform}/node > "$driverDir/node"
    chmod +x "$driverDir/node"
    install -Dm755 target/spel $out/bin/spel
    wrapProgram $out/bin/spel \
      --set-default PLAYWRIGHT_BROWSERS_PATH ${playwright-driver.browsers} \
      --set-default SPEL_DRIVER_DIR $out/share/spel/driver

    runHook postInstall
  '';

  passthru.locker = locked.commandLocker ''
    export HOME="$tmp/home"
    export GITLIBS="$HOME/.gitlibs"
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    ${clojureWithGraal}/bin/clojure -T:build uberjar
  '';

  meta = {
    description = "Clojure Playwright library and browser automation CLI";
    homepage = "https://github.com/Blockether/spel";
    changelog = "https://github.com/Blockether/spel/blob/${rev}/CHANGELOG.md";
    license = lib.licenses.asl20;
    inherit (graalvm.meta) platforms;
    mainProgram = "spel";
  };
}
