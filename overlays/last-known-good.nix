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
  #inherit
  #  (nixpkgs {
  #    rev = "a27925d47cf4641e2f01d854385b505988723b5e";
  #    sha256 = "sha256-Ar31/wIvszaIPZyG/8sugnSUExN8XVcl77v80YhnnHQ=";
  #    config = self.config;
  #  })
  #  beets-unstable
  #  ;
  inherit
    (nixpkgs {
      rev = "07f09fcbbb58ae91af01f451aea51b6b5a12b21f";
      sha256 = "sha256-Jw5cmJ7PvNMytGtkeN6yPTJ3+eUhE4dcU9d8WR+8fWo=";
      config = self.config;
    })
    mullvad
    ;

  # ref: https://nixpk.gs/pr-tracker.html?pr=381214
  inherit
    (nixpkgs {
      rev = "b71d04bcc6b5a5526ed493e1c8dda89c9d381e7f";
      sha256 = "sha256-mvHR8I9HMr0inknIPOxzwHYAj2wk61LEEy1Ju1ISt24=";
      config = self.config;
    })
    ntopng
    ;

  # ref: https://nixpk.gs/pr-tracker.html?pr=380045
  inherit
    (nixpkgs {
      rev = "06f9bc9aa688754d9e7cd72ef21e38ced1c5748b";
      sha256 = "sha256-ZVdi1juNUXY8O9Sa3JUYapbpFoGA9UI+C5gEKwtnl9Q=";
      config = self.config;
    })
    sonarr
    ;

  # ref: https://nixpk.gs/pr-tracker.html?pr=377988
  inherit
    (nixpkgs {
      rev = "5bff874fd63b4c972c289a4f3ca30a7f90d17420";
      sha256 = "sha256-01hwIoQDMKTSOAp0i0GpCOXhdVxyav5NurRHKk0vOAc=";
      config = self.config;
    })
    authentik
    ;
}
