# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
_final: prev:

let
  nixpkgs =
    #args@{ rev, sha256 }:
    args:
    import (prev.fetchFromGitHub (
      args
      // {
        owner = "NixOS";
        repo = "nixpkgs";
      }
    )) { inherit (prev.stdenv.hostPlatform) system; };
in
{
  inherit
    (nixpkgs {
      rev = "5e35f8875cfa5871cf7af29575dd9186b615a314";
      sha256 = "sha256-WOxcqjpSEh8tUGUNJ6CSal4qGCDWruKhBPvPF0sZYcg=";
    })
    authentik
    ;

  #inherit
  #  (nixpkgs {
  #    rev = "5e35f8875cfa5871cf7af29575dd9186b615a314";
  #    sha256 = "3d1ccb8d3095393e361d10207188037b592e3d91";
  #    config = final.config;
  #  })
  #  authentik
  #  ;

}
