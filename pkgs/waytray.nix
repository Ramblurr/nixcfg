{
  lib,
  stdenv,
  fetchFromGitHub,
  rustPlatform,
  pkg-config,
  wrapGAppsHook4,
  gtk4,
  gdk-pixbuf,
  glib,
  gtk4-layer-shell,
  gsettings-desktop-schemas,
  gst_all_1,
  ...
}:

rustPlatform.buildRustPackage rec {
  pname = "waytray";
  version = "unstable-2026-01-06";

  src = fetchFromGitHub {
    owner = "destructatron";
    repo = "waytray";
    rev = "ccd6ac471754ff951af128e5df4033702cf2cbe1";
    hash = "sha256-qf6t5Y9gA9uF6Ku/J6zdsJ0oohX5xCjdyZiPlJPPln0=";
  };

  cargoLock = {
    lockFile = ./waytray.Cargo.lock;
  };

  nativeBuildInputs = [
    pkg-config
    wrapGAppsHook4
  ];

  buildInputs = [
    gtk4
    gdk-pixbuf
    glib
    gtk4-layer-shell
    gsettings-desktop-schemas
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
  ];

  cargoBuildFlags = [ "--workspace" ];

  postPatch = ''
    cp ${./waytray.Cargo.lock} Cargo.lock
  '';

  installPhase = ''
    runHook preInstall

    bin_dir="target/${stdenv.hostPlatform.rust.rustcTarget}/release"
    if [ ! -d "$bin_dir" ]; then
      bin_dir="target/release"
    fi

    install -Dm755 "$bin_dir/waytray" $out/bin/waytray
    install -Dm755 "$bin_dir/waytray-daemon" $out/bin/waytray-daemon

    runHook postInstall
  '';

  meta = with lib; {
    description = "Accessible, compositor-agnostic system tray for Linux";
    homepage = "https://github.com/destructatron/waytray";
    license = licenses.mit;
    mainProgram = "waytray";
    platforms = platforms.linux;
  };
}
