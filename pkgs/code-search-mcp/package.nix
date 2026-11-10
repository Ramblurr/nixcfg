{
  lib,
  stdenvNoCC,
  babashka,
  clojure,
  git,
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
  locked = (import "${clojureNixLocker}/default.nix" { inherit pkgs; }).lockfile {
    src = dependencySource;
    lockfile = "deps.lock.json";
  };
in
assert lib.assertMsg (lib.versionAtLeast babashka.version "1.13.226")
  "code-search-mcp requires Babashka 1.13.226 or newer";
stdenvNoCC.mkDerivation {
  pname = "code-search-mcp";
  version = "0.2.0";
  src = dependencySource;
  nativeBuildInputs = [
    clojure
    makeWrapper
  ];
  nativeInstallCheckInputs = [
    babashka
    git
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
    app="$out/share/code-search-mcp"
    mkdir -p "$app/lib"
    install -m644 ${./code-search-mcp.clj} "$app/code-search-mcp.clj"
    printf '{}\n' > "$app/bb.edn"
    IFS=: read -r -a entries < classpath
    classpath=""
    index=0
    for entry in "''${entries[@]}"; do
      # tools.deps can include an absent default src directory.
      if [ ! -e "$entry" ]; then continue; fi
      target="$app/lib/$index-$(basename "$entry")"
      cp -rL "$entry" "$target"
      classpath="''${classpath:+$classpath:}$target"
      index=$((index + 1))
    done
    printf '%s\n' "$classpath" > "$app/classpath"
    makeWrapper ${babashka}/bin/bb "$out/bin/code-search-mcp" \
      --set CODE_SEARCH_MCP_PACKAGED 1 \
      --prefix PATH : ${lib.makeBinPath [ git ]} \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ sqlite ]} \
      --add-flags "--config $app/bb.edn --classpath $classpath $app/code-search-mcp.clj"
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    mkdir -p fixture/{tests,pkgs/code-search-mcp}
    cp ${../../tests/code-search-mcp_test.clj} fixture/tests/code-search-mcp_test.clj
    cp "$out/share/code-search-mcp/code-search-mcp.clj" fixture/pkgs/code-search-mcp/
    export HOME="$TMPDIR/test-home"
    mkdir -p "$HOME"
    export CODE_SEARCH_MCP_PACKAGED=1
    export LD_LIBRARY_PATH=${lib.makeLibraryPath [ sqlite ]}
    ${babashka}/bin/bb --config "$out/share/code-search-mcp/bb.edn" \
      --classpath "$(< "$out/share/code-search-mcp/classpath")" \
      fixture/tests/code-search-mcp_test.clj
    runHook postInstallCheck
  '';

  passthru.locker = locked.commandLocker ''
    export HOME="$tmp/home"
    export GITLIBS="$HOME/.gitlibs"
    unset CLJ_CACHE CLJ_CONFIG XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME
    ${clojure}/bin/clojure -J-Xmx1g -Srepro -P
  '';

  meta = {
    description = "Read-only MCP search and Git access over local dependency mirrors";
    mainProgram = "code-search-mcp";
    platforms = lib.platforms.linux;
  };
}
