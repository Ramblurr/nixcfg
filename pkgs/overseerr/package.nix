{
  lib,
  mkYarnPackage,
  fetchFromGitHub,
  fetchYarnDeps,
  nodejs,
  python3,
  makeWrapper,
}:

mkYarnPackage rec {
  pname = "overseerr";
  version = "1.33.2";
  #version = "1.34.0";

  src = fetchFromGitHub {
    owner = "sct";
    repo = "overseerr";
    rev = "v${version}";
    hash = "sha256-xDzWyU4f56+0Tpk87LpH6zXtxmRxVMCKySCY6WD5go0=";
  };

  offlineCache = fetchYarnDeps {
    yarnLock = ./yarn.lock;
    hash = "sha256-f30P+/DxDz9uBmdgvaYK4YOAUmVce8MUnNHBXr8/yKc=";
  };

  env = {
    CYPRESS_INSTALL_BINARY = 0; # cypress tries to download binaries otherwise
  };

  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;

  nativeBuildInputs = [
    nodejs
    makeWrapper
  ];

  # Fixes "SQLite package has not been found installed" at launch
  pkgConfig.sqlite3 = {
    nativeBuildInputs = [
      nodejs.pkgs.node-pre-gyp
      python3
    ];
    postInstall = ''
      export CPPFLAGS="-I${nodejs}/include/node"
      node-pre-gyp install --prefer-offline --build-from-source --nodedir=${nodejs}/include/node
      rm -r build-tmp-napi-v6
    '';
  };

  # Fixes MODULE_NOT_FOUND at launch.
  pkgConfig.bcrypt = {
    nativeBuildInputs = [
      nodejs.pkgs.node-pre-gyp
      python3
    ];
    postInstall = ''
      export CPPFLAGS="-I${nodejs}/include/node"
      node-pre-gyp install --prefer-offline --build-from-source --nodedir=${nodejs}/include/node
    '';
  };

  postInstall = ''
    makeWrapper '${nodejs}/bin/node' "$out/bin/overseerr" \
      --add-flags "$out/libexec/overseerr/deps/overseerr/dist/index.js" \
      --set NODE_ENV production
  '';

  buildPhase = ''
    runHook preBuild
    (
      shopt -s dotglob
      cd deps/overseerr
      rm -r config/*
      yarn --offline build
      rm -r .next/cache
    )
    runHook postBuild
  '';

  distPhase = "true";

  meta = {
    description = "Request management and media discovery tool for the Plex ecosystem";
    homepage = "https://github.com/sct/overseerr";
    license = lib.licenses.mit;
    mainProgram = "start";
    maintainers = with lib.maintainers; [ caarlos0 ];
  };
}
