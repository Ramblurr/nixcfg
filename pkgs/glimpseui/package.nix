{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
  chromium,
  at-spi2-core,
  gdk-pixbuf,
  glib,
  gobject-introspection,
  gtk3,
  harfbuzz,
  libayatana-appindicator,
  libx11,
  libxfixes,
  pango,
  python3,
  socat,
  xdg-utils,
  xdotool,
  xprop,
}:

let
  python = python3.withPackages (pythonPackages: [ pythonPackages.pygobject3 ]);
  runtimePath = lib.makeBinPath [
    python
    socat
    xdg-utils
    xdotool
    xprop
  ];
  giTypelibPath = lib.makeSearchPath "lib/girepository-1.0" (
    map lib.getLib [
      at-spi2-core
      gdk-pixbuf
      glib
      gobject-introspection
      gtk3
      harfbuzz
      libayatana-appindicator
      pango
    ]
  );
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "glimpseui";
  version = "0.8.1";

  src = fetchFromGitHub {
    owner = "HazAT";
    repo = "glimpse";
    rev = "v${finalAttrs.version}";
    hash = "sha256-iiOLxg8UnsKPwqNV+zCLFoQZ78pypMr3WkesSf3nkc8=";
  };

  nativeBuildInputs = [ makeWrapper ];

  postPatch = ''
    substituteInPlace src/chromium-backend.mjs \
      --replace-fail \
        "if (process.env.GLIMPSE_CHROME_PATH) return process.env.GLIMPSE_CHROME_PATH;" \
        "if (process.env.GLIMPSE_CHROME_PATH) return process.env.GLIMPSE_CHROME_PATH;
         const packagedChromium = '${lib.getExe chromium}';
         if (existsSync(packagedChromium)) return packagedChromium;" \
      --replace-fail \
        "x11 = ctypes.cdll.LoadLibrary(ctypes.util.find_library('X11'))" \
        "x11 = ctypes.cdll.LoadLibrary('${lib.getLib libx11}/lib/libX11.so.6')" \
      --replace-fail \
        "xfixes = ctypes.cdll.LoadLibrary(ctypes.util.find_library('Xfixes'))" \
        "xfixes = ctypes.cdll.LoadLibrary('${lib.getLib libxfixes}/lib/libXfixes.so.3')"
  '';

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    packageRoot="$out/lib/node_modules/glimpseui"
    mkdir -p "$packageRoot/bin" "$packageRoot/src" "$out/bin"
    install -m0755 bin/glimpse.mjs "$packageRoot/bin/glimpse.mjs"
    install -m0644 src/*.mjs "$packageRoot/src/"
    install -m0644 package.json package-lock.json README.md CHANGELOG.md LICENSE "$packageRoot/"
    cp -R skills "$packageRoot/skills"

    makeWrapper ${nodejs}/bin/node "$out/bin/glimpseui" \
      --add-flags "$packageRoot/bin/glimpse.mjs" \
      --unset GLIMPSE_BINARY_PATH \
      --unset GLIMPSE_HOST_PATH \
      --set GLIMPSE_BACKEND chromium \
      --set GLIMPSE_CHROME_PATH "${lib.getExe chromium}" \
      --prefix GI_TYPELIB_PATH : "${giTypelibPath}" \
      --prefix PATH : "${runtimePath}"

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck

    GLIMPSE_BACKEND=native \
      GLIMPSE_BINARY_PATH=/not-a-native-host \
      GLIMPSE_HOST_PATH=/not-another-native-host \
      GLIMPSE_CHROME_PATH=/not-a-browser \
      GLIMPSE_EXPECTED_CHROME_PATH="${lib.getExe chromium}" \
      GLIMPSE_EXPECTED_LIBX11_PATH="${lib.getLib libx11}/lib/libX11.so.6" \
      GLIMPSE_EXPECTED_LIBXFIXES_PATH="${lib.getLib libxfixes}/lib/libXfixes.so.3" \
      GLIMPSE_MODULE_PATH="file://$out/lib/node_modules/glimpseui/src/glimpse.mjs" \
      NODE_OPTIONS="--import=${./test-backend.mjs}" \
      "$out/bin/glimpseui" --help | grep -F "Glimpse" >/dev/null

    runHook postInstallCheck
  '';

  meta = {
    description = "Native micro-UI for scripts and agents using Chromium CDP";
    homepage = "https://github.com/HazAT/glimpse";
    changelog = "https://github.com/HazAT/glimpse/blob/v${finalAttrs.version}/CHANGELOG.md";
    license = lib.licenses.mit;
    mainProgram = "glimpseui";
    platforms = lib.platforms.linux;
  };
})
