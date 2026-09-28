{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_24,
  electron,
  makeWrapper,
  makeDesktopItem,
  copyDesktopItems,
  runCommand,
  xvfb-run,
}:
buildNpmPackage (finalAttrs: {
  pname = "keybr-standalone";
  version = "0.0.0-unstable-2026-07-08";

  src = fetchFromGitHub {
    owner = "aedisluna";
    repo = "Keybr-standalone";
    rev = "6156d44b89529fc16cb490312fd33a44804d1460";
    hash = "sha256-hXcGJT5LHyvb8l27oRupj0bBUW055ih/TN7KMta5IBc=";
  };
  nodejs = nodejs_24;
  npmDepsHash = "sha256-Vtjn24xsU+getjPtGFA8neYtoppjdw2ERhCfLiRwx/c=";
  npmDepsFetcherVersion = 2;
  npmFlags = [ "--ignore-scripts" ];
  env.npm_config_ignore_scripts = "true";

  nativeBuildInputs = [
    makeWrapper
    copyDesktopItems
  ];

  postPatch = ''
    cp ${./xdg.cjs} desktop/xdg.cjs
    substituteInPlace desktop/main.js \
      --replace-fail 'const HOST = "127.0.0.1";' 'const { data: dataDir } = require("./xdg.cjs"); const HOST = "127.0.0.1";' \
      --replace-fail 'path.join(app.getPath("userData"), "data")' 'path.join(dataDir, "data")' \
      --replace-fail 'path.join(app.getPath("userData"), "database.sqlite")' 'path.join(dataDir, "database.sqlite")' \
      --replace-fail 'spawn(process.execPath, [serverEntry]' 'spawn("${nodejs_24}/bin/node", [serverEntry]'
    substituteInPlace packages/server/lib/server/service.ts \
      --replace-fail 'this.#server.listen(port);' 'this.#server.listen(port, "127.0.0.1");'
  '';

  buildPhase = ''
    runHook preBuild
    # Apply upstream source patches without running its postinstall/husky hooks.
    for patchFile in patches/*.patch; do
      patch -p1 < "$patchFile"
    done
    # Build the sole native runtime dependency against the separate Node server,
    # not Electron's ABI. No prebuilt downloads or npm lifecycle scripts.
    (cd node_modules/better-sqlite3 && \
      node ${nodejs_24}/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js \
        rebuild --nodedir=${nodejs_24})
    NODE_ENV=production node node_modules/webpack-cli/bin/cli.js
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    appRoot="$out/share/keybr-standalone"
    mkdir -p "$appRoot/desktop/node_modules" "$appRoot/root" "$out/bin"
    cp -r root/lib root/public "$appRoot/root/"
    cp desktop/{main.js,preload.js,package.json,xdg.cjs} "$appRoot/desktop/"
    cp -r node_modules/{better-sqlite3,bindings,file-uri-to-path} "$appRoot/desktop/node_modules/"
    cp ${./server.cjs} "$appRoot/server.cjs"
    makeWrapper ${lib.getExe nodejs_24} "$out/bin/keybr" \
      --add-flags "$appRoot/server.cjs" \
      --set NODE_PATH "$appRoot/desktop/node_modules"
    find "$appRoot" -name '*.map' -delete
    makeWrapper ${lib.getExe electron} "$out/bin/keybr-standalone" \
      --add-flags "$appRoot/desktop" \
      --unset ELECTRON_RUN_AS_NODE
    runHook postInstall
  '';

  desktopItems = [
    (makeDesktopItem {
      name = "keybr-standalone";
      desktopName = "Keybr";
      comment = "Offline typing practice";
      exec = "keybr-standalone";
      categories = [ "Education" ];
    })
  ];

  passthru.tests.smoke =
    runCommand "keybr-standalone-smoke"
      {
        nativeBuildInputs = [
          electron
          xvfb-run
        ];
      }
      ''
        export APP_ROOT=${finalAttrs.finalPackage}/share/keybr-standalone
        unset ELECTRON_RUN_AS_NODE
        for mode in default custom relative; do
          export HOME="$TMPDIR/$mode/home"
          mkdir -p "$HOME"
          unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME TEST_XDG_BASE
          if [ "$mode" = custom ]; then
            export TEST_XDG_BASE="$TMPDIR/custom paths"
            export XDG_CONFIG_HOME="$TEST_XDG_BASE/config"
            export XDG_DATA_HOME="$TEST_XDG_BASE/data"
            export XDG_CACHE_HOME="$TEST_XDG_BASE/cache"
            export XDG_STATE_HOME="$TEST_XDG_BASE/state"
          elif [ "$mode" = relative ]; then
            export XDG_CONFIG_HOME=relative XDG_DATA_HOME=relative
            export XDG_CACHE_HOME=relative XDG_STATE_HOME=relative
          fi
          # Chromium's sandbox cannot nest in the Nix build sandbox. The installed
          # launcher does not disable Chromium's sandbox.
          xvfb-run -a electron --no-sandbox --disable-gpu ${./smoke-test.cjs}
        done
        touch "$out"
      '';

  passthru.tests.server = runCommand "keybr-server-test" { } ''
    ${lib.getExe nodejs_24} ${./server-test.cjs} ${finalAttrs.finalPackage}/bin/keybr
    touch "$out"
  '';

  meta = {
    description = "Offline Keybr typing practice desktop application";
    homepage = "https://github.com/aedisluna/Keybr-standalone";
    license = lib.licenses.agpl3Only;
    mainProgram = "keybr-standalone";
    platforms = lib.platforms.linux;
  };
})
