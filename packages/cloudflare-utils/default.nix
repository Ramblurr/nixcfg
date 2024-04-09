{
  pkgs,
  lib,
  stdenv,
  fetchFromGitHub,
}:
pkgs.buildGoModule rec {
  name = "cloudflare-utils";
  version = "1.2.1";
  src = pkgs.fetchFromGitHub {
    owner = "Cyb3r-Jak3";
    repo = "cloudflare-utils";
    rev = "v${version}";
    sha256 = "";
  };
  vendorHash = "";
  meta = with lib; {
    description = "Helpful Cloudflare utility program";
    homepage = "https://github.com/Cyb3r-Jak3/cloudflare-utils";
    license = licenses.apache;
    maintainers = with maintainers; [ ];
  };
}
