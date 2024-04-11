# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
self: pkgs:

let
  nixpkgs =
    args@{ rev, sha256 }:
    import (pkgs.fetchFromGitHub (
      args
      // {
        owner = "NixOS";
        repo = "nixpkgs";
      }
    )) { system = self.system; };
in
{

  # https://nixpk.gs/pr-tracker.html?pr=302544
  inherit
    (nixpkgs {
      rev = "309cf7f3e44723d6f65355579fbe10c82202fe8d";
      sha256 = "sha256-Wtf9WbMBljUaAHPIzvN95em/54n/oaxWaBP2XkiOFZk=";
    })
    electron_28
    electron_27
    ;
}
