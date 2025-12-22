{
  fetchFromGitHub,
  lib,
  rustPlatform,
  writeShellScript,
  udev,
  pkg-config,
}:
rustPlatform.buildRustPackage {
  name = "swhkd";
  version = "unstable-2024-09-23";
  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "swhkd";
    rev = "f2bee30146eda2b7f10347ce93f3c2236f73ae93";
    hash = "sha256-0b0vod0CxLLMFS+28kbpfYxLjyok3lioz3oiKSUTzoU=";
  };
  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "sweet-0.3.0" = "sha256-swSE1CE39cGojp8HAziw0Bzjr+s4XaVU+4OqQDO60fE=";
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
