# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
final: prev:

let
  nixpkgs =
    args@{
      rev,
      sha256,
      config ? { },
    }:
    import
      (prev.fetchFromGitHub (
        removeAttrs args [ "config" ]
        // {
          owner = "NixOS";
          repo = "nixpkgs";
        }
      ))
      {
        inherit config;
        system = final.system;
      };
in
{
}
