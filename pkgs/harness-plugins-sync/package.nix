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
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    app="$out/share/harness-plugins-sync"
    mkdir -p "$app/lib"
    cp -r ${sourceFiles}/src "$app/src"
    install -m644 ${sourceFiles}/NOTICE "$app/NOTICE"
    printf '{}\n' > "$app/bb.edn"
    IFS=: read -r -a entries < classpath
    classpath="$app/src"
    index=0
    for entry in "''${entries[@]}"; do
      if [ ! -e "$entry" ]; then continue; fi
      target="$app/lib/$index-$(basename "$entry")"
      cp -rL "$entry" "$target"
      classpath="$classpath:$target"
      index=$((index + 1))
    done
    printf '%s\n' "$classpath" > "$app/classpath"
    makeWrapper ${babashka}/bin/bb "$out/bin/harness-plugins-sync" \
      --prefix PATH : ${
        lib.makeBinPath [
          coreutils
          git
          gnutar
          openssh
        ]
      } \
      --add-flags "--config $app/bb.edn --classpath $classpath -m harness-plugins-sync.main"
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
    ${babashka}/bin/bb --config "$out/share/harness-plugins-sync/bb.edn" \
      --classpath "$(< "$out/share/harness-plugins-sync/classpath"):$TMPDIR/test" \
      -e "(require 'clojure.test 'harness-plugins-sync.cli-test 'harness-plugins-sync.core-test) (let [r (clojure.test/run-tests 'harness-plugins-sync.cli-test 'harness-plugins-sync.core-test)] (System/exit (+ (:fail r) (:error r))))"

    # Verify the installed standalone binary runs outside the checkout and beside an unrelated bb.edn.
    mkdir -p "$TMPDIR/unrelated"
    printf '{:deps {nonexistent/pkg {:mvn/version "0.0.0"}}}\n' > "$TMPDIR/unrelated/bb.edn"
    (cd "$TMPDIR/unrelated" && "$out/bin/harness-plugins-sync" help >/dev/null)
    (cd "$TMPDIR/unrelated" && "$out/bin/harness-plugins-sync" doctor --json >/dev/null)
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
