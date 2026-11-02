{
  lib,
  rustPlatform,
  fetchFromGitHub,
  ...
}:

rustPlatform.buildRustPackage rec {
  pname = "netns-proxy";
  version = "0.2.1-unstable-2026-10-22";

  src = fetchFromGitHub {
    owner = "fooker";
    repo = "netns-proxy";
    rev = "20e010e8a9d9dc2c7d6eec010c5df83fb299cfe0";
    sha256 = "sha256-KBFr+eXQGiSDN3oPsj794p4oC2kJ4l+iVurIY9fp2NA=";
  };

  cargoLock = {
    lockFile = src + /Cargo.lock;
  };

  meta = with lib; {
    description = "A simple and slim proxy to forward ports from and into linux network namespaces";
    homepage = "https://github.com/fooker/netns-proxy";
    license = licenses.mit;
  };
}
