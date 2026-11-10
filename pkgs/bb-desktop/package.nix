{
  lib,
  appimageTools,
  fetchurl,
  makeWrapper,
  pkgs-lib,
  git,
  gh,
  nix,
}:

let
  pname = "bb-desktop";
  version = "0.49.0";
  src = fetchurl {
    url = "https://github.com/get-bb/bb/releases/download/desktop-v${version}/bb-${version}-x86_64.AppImage";
    hash = "sha256-g0mrdzcVYk/sUIJznIF9KhWAlQRpnTS0fT2d++YPx3A=";
  };
  contents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraPkgs = pkgs: [
    pkgs.git
    pkgs.libsecret
  ];
  nativeBuildInputs = [ makeWrapper ];

  extraInstallCommands = ''
    # Keep the existing bb CLI on PATH. Nix owns updates to this package.
    wrapProgram $out/bin/bb-desktop \
      --unset APPIMAGE \
      --set BB_HOST_DAEMON_AUTO_UPDATE 0

    install -Dm644 ${contents}/bb.desktop $out/share/applications/bb-desktop.desktop
    substituteInPlace $out/share/applications/bb-desktop.desktop \
      --replace-fail "Exec=AppRun %U" "Exec=bb-desktop %U" \
      --replace-fail "Icon=bb" "Icon=bb-desktop"
    install -Dm644 ${contents}/bb.png $out/share/icons/hicolor/512x512/apps/bb-desktop.png
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "bb-desktop";
    utils = [
      git
      gh
      nix
    ];
    script = ./update.bb;
  };

  meta = {
    description = "BB desktop app with its bundled server and host daemon";
    homepage = "https://github.com/get-bb/bb";
    license = lib.licenses.mit;
    mainProgram = "bb-desktop";
    platforms = [ "x86_64-linux" ];
  };
}
