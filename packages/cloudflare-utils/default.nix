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
    sha256 = "sha256-41TQ+St6U4exLSl4dwc1E6K8P+oqQ4m5RSI7L2/dWwI=";
  };
  vendorHash = "sha256-HE6x4KSe9b9ZzcYz7sP25aTeDGU4zXgkYm/1RwYYBt4=";
  meta = with lib; {
    description = "Helpful Cloudflare utility program";
    homepage = "https://github.com/Cyb3r-Jak3/cloudflare-utils";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
