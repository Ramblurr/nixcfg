{
  pkgs,
  lib,
  stdenv,
  fetchFromGitHub,
}:
pkgs.buildGoModule rec {
  name = "hacompanion";
  version = "1.0.7";
  src = pkgs.fetchFromGitHub {
    owner = "tobias-kuendig";
    repo = "hacompanion";
    rev = "8f8cbefa30f3def757104bd411de341dea459dae";
    sha256 = "sha256-M590LdSMI/C9B8ZNeE9k1ZolyBY0vs4cUSAZYpd/ECQ=";
  };
  vendorHash = "sha256-ZZ8nxN+zUeFhSXyoHLMgzeFllnIkKdoVnbVK5KjrLEQ=";
  meta = with lib; {
    description = "Daemon that sends local hardware information to Home Assistant Resources";
    homepage = "https://github.com/tobias-kuendig/hacompanion";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
