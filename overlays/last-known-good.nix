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

  inherit
    (nixpkgs {
      rev = "3166af96391de5ec29a8a1aeb80c2e5c1f3e243a";
      sha256 = "";
    })
    musescore
    ;

  # Workaround kernel bug in kernel >= 6.6.57
  # ref: https://github.com/NixOS/nixpkgs/issues/353709
  # must add to config:
  # boot.kernelPackages = pkgs.linuxPackages_6_6;
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  inherit
    (nixpkgs {
      rev = "b72f50d54d0d0e7428cb39cd39f29e7ed2e7e5ea";
      sha256 = "sha256-2ipWz8DPaUgOkskpHRSH9su1kon1/XtwozXaKewfCtU=";
      config = self.config;
    })
    linuxPackages_6_6
    ;
}
