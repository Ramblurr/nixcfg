{
  lib,
  stdenvNoCC,
  babashka,
  clojure,
  fetchurl,
  sqlite,
  makeWrapper,
  clojureNixLocker,
  pkgs,
}:
let
  dependencySource = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./deps.edn
      (lib.fileset.maybeMissing ./deps.lock.json)
    ];
  };
  sourceFiles = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./src
      ./test
    ];
  };
  locked = (import "${clojureNixLocker}/default.nix" { inherit pkgs; }).lockfile {
    src = dependencySource;
    lockfile = "deps.lock.json";
  };
  datastar = fetchurl {
    url = "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js";
    hash = "sha256-cnhErfyCXuZR+5PFRKKnOZhvmiGCCpRSSzXwysRwz5E=";
  };
  libraryPath = lib.makeLibraryPath [ sqlite ];
in
assert lib.assertMsg (lib.versionAtLeast babashka.version "1.13.229")
  "ts-broker requires Babashka 1.13.229 or newer for babashka.ffi";
stdenvNoCC.mkDerivation {
  pname = "ts-broker";
  version = "0.1.0";
  src = dependencySource;
  nativeBuildInputs = [
    clojure
    makeWrapper
  ];
  nativeInstallCheckInputs = [ babashka ];
  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    source ${locked.shellEnv}
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    clojure -J-Xmx1g -Srepro -Spath > classpath
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    app="$out/share/ts-broker"
    mkdir -p "$app/lib" "$out/bin"
    cp -r ${sourceFiles}/src "$app/src"
    printf '{}\n' > "$app/bb.edn"
    IFS=: read -r -a entries < classpath
    classpath="$app/src"
    index=0
    for entry in "''${entries[@]}"; do
      # tools.deps lists the absent relative src path; the copy above replaces it.
      if [ ! -e "$entry" ] || [ "$entry" = src ]; then continue; fi
      target="$app/lib/$index-$(basename "$entry")"
      cp -rL "$entry" "$target"
      classpath="$classpath:$target"
      index=$((index + 1))
    done
    printf '%s\n' "$classpath" > "$app/classpath"
    makeWrapper ${babashka}/bin/bb "$out/bin/ts-broker" \
      --set TS_BROKER_DATASTAR ${datastar} \
      --prefix LD_LIBRARY_PATH : ${libraryPath} \
      --add-flags "--config $app/bb.edn --classpath $classpath -m ts-broker.main"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    export HOME="$TMPDIR/test-home"
    mkdir -p "$HOME"
    export LD_LIBRARY_PATH=${libraryPath}
    ${babashka}/bin/bb --config "$out/share/ts-broker/bb.edn" \
      --classpath "$(< "$out/share/ts-broker/classpath"):${sourceFiles}/test" \
      -e "(require 'ts-broker.test-runner) (System/exit (ts-broker.test-runner/run))"
    "$out/bin/ts-broker" help | grep -q 'ts-broker serve'
    runHook postInstallCheck
  '';

  passthru.locker = locked.commandLocker ''
    export HOME="$tmp/home"
    export GITLIBS="$HOME/.gitlibs"
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    ${clojure}/bin/clojure -J-Xmx1g -Srepro -P
  '';

  meta = {
    description = "Approval broker that tells ts-egress which credential an agent may use";
    mainProgram = "ts-broker";
    platforms = lib.platforms.linux;
  };
}
