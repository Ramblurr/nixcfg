{
  lib,
  stdenvNoCC,
  babashka,
  clojure,
  coreutils,
  git,
  gnutar,
  makeWrapper,
  openssh,
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
      ./NOTICE
      ./src
      ./test
    ];
  };
  locked = (import "${clojureNixLocker}/default.nix" { inherit pkgs; }).lockfile {
    src = dependencySource;
    lockfile = "deps.lock.json";
  };
in
assert lib.assertMsg (lib.versionAtLeast babashka.version "1.13.226")
  "harness-plugins-sync requires Babashka 1.13.226 or newer";
stdenvNoCC.mkDerivation {
  pname = "harness-plugins-sync";
  version = "0.1.0";
  src = dependencySource;
  nativeBuildInputs = [
    babashka
    clojure
    makeWrapper
  ];
  nativeInstallCheckInputs = [
    babashka
    coreutils
    git
    gnutar
  ];
  dontConfigure = true;

  buildPhase = ''
    runHook preBuild
    source ${locked.shellEnv}
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    clojure -J-Xmx1g -Srepro -Spath > classpath
    IFS=: read -r -a entries < classpath
    full_cp="${sourceFiles}/src"
    for entry in "''${entries[@]}"; do
      if [ ! -e "$entry" ]; then continue; fi
      full_cp="$full_cp:$entry"
    done
    printf '%s\n' "$full_cp" > test-classpath
    ${babashka}/bin/bb --config /dev/null --classpath "$full_cp" \
      uberscript uberscript.clj -m harness-plugins-sync.main
    {
      printf '#!/usr/bin/env -S bb --config /dev/null\n'
      cat uberscript.clj
    } > harness-plugins-sync
    chmod +x harness-plugins-sync
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    app="$out/share/harness-plugins-sync"
    mkdir -p "$app" "$out/bin"
    install -m755 harness-plugins-sync "$app/harness-plugins-sync"
    install -m644 ${sourceFiles}/NOTICE "$app/NOTICE"
    makeWrapper ${babashka}/bin/bb "$out/bin/harness-plugins-sync" \
      --prefix PATH : ${
        lib.makeBinPath [
          coreutils
          git
          gnutar
          openssh
        ]
      } \
      --add-flags "--config /dev/null $app/harness-plugins-sync"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    export HOME="$TMPDIR/test-home"
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_DATA_HOME="$HOME/.local/share"
    mkdir -p "$HOME"
    cp -r ${sourceFiles}/test "$TMPDIR/test"
    ${babashka}/bin/bb --config /dev/null \
      --classpath "$(< test-classpath):$TMPDIR/test" \
      -e "(require 'clojure.test 'harness-plugins-sync.cli-test 'harness-plugins-sync.core-test) (let [r (clojure.test/run-tests 'harness-plugins-sync.cli-test 'harness-plugins-sync.core-test)] (System/exit (+ (:fail r) (:error r))))"

    # Verify the installed standalone binary runs outside the checkout and beside an unrelated bb.edn.
    mkdir -p "$TMPDIR/unrelated"
    printf '{:deps {nonexistent/pkg {:mvn/version "0.0.0"}}}\n' > "$TMPDIR/unrelated/bb.edn"
    (cd "$TMPDIR/unrelated" && "$out/bin/harness-plugins-sync" help >/dev/null)
    (cd "$TMPDIR/unrelated" && "$out/bin/harness-plugins-sync" doctor --json >/dev/null)
    (cd "$TMPDIR/unrelated" && "$out/bin/harness-plugins-sync" sync --schema all --json >/dev/null)
    runHook postInstallCheck
  '';

  passthru.locker = locked.commandLocker ''
    export HOME="$tmp/home"
    export GITLIBS="$HOME/.gitlibs"
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    ${clojure}/bin/clojure -J-Xmx1g -Srepro -P
  '';

  meta = {
    description = "Sync agent plugin packs and bind them natively across harnesses and profiles";
    mainProgram = "harness-plugins-sync";
    platforms = lib.platforms.linux;
  };
}
