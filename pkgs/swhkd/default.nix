{
  fetchFromGitHub,
  lib,
  rustPlatform,
  udev,
  pkg-config,
}:
rustPlatform.buildRustPackage {
  name = "swhkd";
  version = "unstable-2026-04-26";
  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "swhkd";
    rev = "cbbfc4a981aa263155e3216a42549c9a3ae645fe";
    hash = "sha256-Q1EaYUfSMKkwKq+GfBDPIjfe4oS0rhwpPD1sS9nvKpI=";
  };
  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "sweet-0.4.0" = "sha256-Ky2afQ5HyO1a6YT8Jjl6az1jczq+MBKeuRmFwmcvg6U=";
    };
  };

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [
    udev
  ];

  postBuild = ''
    $src/scripts/build-polkit-policy.sh \
    --swhkd-path=$out/bin/swhkd
  '';

  postInstall = ''
    install -D ./com.github.swhkd.pkexec.policy -t $out/share/polkit-1/actions
  '';

  meta = with lib; {
    description = "Sxhkd clone for Wayland (works on TTY and X11 too)";
    homepage = "https://github.com/waycrate/swhkd";
    license = licenses.bsd2;
    maintainers = [ maintainers.uningan ];
  };
}
