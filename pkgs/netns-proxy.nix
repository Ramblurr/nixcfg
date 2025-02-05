{
  lib,
  rustPlatform,
  fetchFromGitHub,
  ...
}:

rustPlatform.buildRustPackage rec {
  pname = "netns-proxy";
  version = "unstable-2023-09-17";

  src = fetchFromGitHub {
    owner = "fooker";
    repo = "netns-proxy";
    rev = "6d9ccbfde4375cd614735ea5f6ee5aba2b6cfd2b";
    sha256 = "sha256-N+my6cTuA7yNoYxocpRiLNcy7OwrJLvO2cGLJGv8a/I=";
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
