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
  version = "unstable-2026-03-20";

  src = fetchFromGitHub {
    owner = "destructatron";
    repo = "waytray";
    rev = "8e7ac948a582c9bd0c6e4c8a8a30cc27aa7763a6";
    hash = "sha256-FoKX/O5Rae3LP7MsJuzryF7QJfstXYVpoHHYcK0L7sM=";
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
