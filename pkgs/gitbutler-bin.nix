{
  stdenv,
  dpkg,
  autoPatchelfHook,
  wrapGAppsHook3,
  fetchurl,
  webkitgtk_4_1,
  libsoup_3,
  lib,
  makeWrapper,
}:
let
  version = "0.16.1";
  build = "2393";
in
stdenv.mkDerivation (finalAttrs: {
  inherit version;
  pname = "gitbutler-bin";
  src = fetchurl {
    url = "https://releases.gitbutler.com/releases/release/${version}-${build}/linux/x86_64/GitButler_${version}_amd64.deb";
    hash = "sha256-/dl06d8Qf7hsCjWGPlOEeSE2KhymGcvWiQwWPKskOsU=";
  };

  unpackPhase = "dpkg-deb -x $src unpack";

  nativeBuildInputs = [
    dpkg
    wrapGAppsHook3
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    webkitgtk_4_1
    libsoup_3
  ];

  # TODO: check that the desktop file points to the right binary! Otherwise, use `substituteInPlace`
  installPhase = ''
    install -Dm755 unpack/usr/bin/gitbutler-tauri $out/bin/gitbutler-tauri
    install -Dm755 unpack/usr/bin/gitbutler-git-setsid $out/bin/gitbutler-git-setsid
    install -Dm755 unpack/usr/bin/gitbutler-git-askpass $out/bin/gitbutler-git-askpass

    cp -r unpack/usr/share $out/share
  '';

  meta = {
    description = "Git client for simultaneous branches on top of your existing workflow";
    license = lib.licenses.fsl11Mit;
    mainProgram = "gitbutler-tauri";
    platforms = [ "x86_64-linux" ];
  };
})
