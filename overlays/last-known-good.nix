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
  inherit
    (nixpkgs {
      rev = "07f09fcbbb58ae91af01f451aea51b6b5a12b21f";
      sha256 = "sha256-Jw5cmJ7PvNMytGtkeN6yPTJ3+eUhE4dcU9d8WR+8fWo=";
      config = final.config;
    })
    mullvad
    ;

  #inherit
  #  (nixpkgs {
  #    rev = "65540e5447fd7837cb403dacf26f40ef7b57683f";
  #    sha256 = "sha256-G9RR8VmxqOUBjAopmuNin8ojaXgsTdOnI3T49jK9Tu0=";
  #    config = final.config;
  #  })
  #  electron_36
  #  ;
}
