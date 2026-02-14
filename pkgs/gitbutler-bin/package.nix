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
  pkgs-lib,
  ast-grep,
  nix,
  curl,
}:
let
  version = "0.19.1";
  build = "2847";
in
stdenv.mkDerivation (_finalAttrs: {
  inherit version;
  pname = "gitbutler-bin";
  src = fetchurl {
    url = "https://releases.gitbutler.com/releases/release/${version}-${build}/linux/x86_64/GitButler_${version}_amd64.deb";
    hash = "sha256-b3g8g5RmSZF4UlTZRI0AJojy5KEHEhakqQx2RMXi/30=";
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

  installPhase = ''
    install -Dm755 unpack/usr/bin/gitbutler-tauri $out/bin/gitbutler-tauri
    install -Dm755 unpack/usr/bin/gitbutler-git-setsid $out/bin/gitbutler-git-setsid
    install -Dm755 unpack/usr/bin/gitbutler-git-askpass $out/bin/gitbutler-git-askpass

    cp -r unpack/usr/share $out/share
  '';

  passthru.updateScript = pkgs-lib.writeUpdateScript {
    packageToUpdate = "gitbutler-bin";
    utils = [
      ast-grep
      nix
      curl
    ];
    script = ./update.bb;
  };

  meta = {
    description = "Git client for simultaneous branches on top of your existing workflow";
    license = lib.licenses.fsl11Mit;
    mainProgram = "gitbutler-tauri";
    platforms = [ "x86_64-linux" ];
  };
})
