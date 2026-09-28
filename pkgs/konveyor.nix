{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  ninja,
  pkg-config,
  kdePackages,
  libxkbcommon,
  libxcb,
  python3,
}:
let
  python = python3.withPackages (ps: [ ps.dbus-python ]);
in
stdenv.mkDerivation {
  pname = "konveyor";
  version = "0.1.0-unstable-2026-09-26";

  src = fetchFromGitHub {
    owner = "DevL0rd";
    repo = "Konveyor";
    rev = "c9fe8c45ee5c915463a20bc099652fdd8d91034d";
    hash = "sha256-1ZCSw6WibmGMMrdx66kOKjyN2J+rfzby34XyLhysRbU=";
  };

  nativeBuildInputs = [
    cmake
    ninja
    pkg-config
    kdePackages.extra-cmake-modules
    kdePackages.wrapQtAppsHook
  ];
  buildInputs = [
    kdePackages.kwin
    kdePackages.kdecoration
    kdePackages.kcoreaddons
    kdePackages.kglobalaccel
    kdePackages.kconfig
    kdePackages.kcolorscheme
    kdePackages.knotifications
    kdePackages.ki18n
    kdePackages.kcmutils
    kdePackages.kservice
    kdePackages.qtbase
    kdePackages.qtdeclarative
    kdePackages.kirigami
    kdePackages.kirigami-addons
    kdePackages.kdeclarative
    kdePackages.layer-shell-qt
    libxkbcommon
    libxcb
    python
  ];

  # The upstream installer also mutates the user's desktop and downloads widgets.
  # Build only the effect, settings module, and CLI through CMake instead.
  cmakeFlags = [ "-DKONVEYOR_BUILD_TESTS=OFF" ];
  postPatch = ''
    substituteInPlace src/cheatsheet/konveyor-cheatsheet.in \
      --replace-fail '#!/usr/bin/env python3' '#!${python}/bin/python3'
  '';

  meta = {
    description = "Scrolling column tiling effect for KDE Plasma";
    homepage = "https://github.com/DevL0rd/Konveyor";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
    mainProgram = "konveyor";
  };
}
