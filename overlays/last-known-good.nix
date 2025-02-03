# Living life on unstable can sometimes be... unstable
# This overlay is a way to pin down a known-good version of nixpkgs for certain packages
self: pkgs:

let
  nixpkgs =
    args@{
      rev,
      sha256,
      config ? { },
    }:
    import
      (pkgs.fetchFromGitHub (
        removeAttrs args [ "config" ]
        // {
          owner = "NixOS";
          repo = "nixpkgs";
        }
      ))
      {
        inherit config;
        system = self.system;
      };
in
{

  # not a "last-known", but actually an unmerged PR that fixes it
  # ref: https://github.com/NixOS/nixpkgs/pull/348697
  #python312Packages = pkgs.python312Packages // {
  #  pyqt6 =
  #    (nixpkgs {
  #      rev = "a4307a33c04135bbfc4b293c1683cc4203109443";
  #      sha256 = "sha256-vY06PrwFxll2h1uZv3C7rouvTgduL+IP/ZCvvLe12yA=";
  #    }).python312Packages.pyqt6;
  #  sip =
  #    (nixpkgs {
  #      rev = "a4307a33c04135bbfc4b293c1683cc4203109443";
  #      sha256 = "sha256-vY06PrwFxll2h1uZv3C7rouvTgduL+IP/ZCvvLe12yA=";
  #    }).python312Packages.sip;
  #};
  # ref: https://nixpk.gs/pr-tracker.html?pr=370234
  #      https://github.com/NixOS/nixpkgs/pull/370234
  #      https://github.com/beetbox/beets/pull/5566
  inherit
    (nixpkgs {
      rev = "a27925d47cf4641e2f01d854385b505988723b5e";
      sha256 = "sha256-Ar31/wIvszaIPZyG/8sugnSUExN8XVcl77v80YhnnHQ=";
      config = self.config;
    })
    beets-unstable
    ;
}
