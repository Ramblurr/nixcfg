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

  # https://nixpk.gs/pr-tracker.html?pr=302544
  #inherit
  #  (nixpkgs {
  #    rev = "309cf7f3e44723d6f65355579fbe10c82202fe8d";
  #    sha256 = "sha256-Wtf9WbMBljUaAHPIzvN95em/54n/oaxWaBP2XkiOFZk=";
  #  })
  #  electron_28
  #  electron_27
  #  ;

  # https://github.com/NixOS/nixpkgs/issues/325832
  #inherit
  #  (nixpkgs {
  #    rev = "757873b3468a1d413d6776c7f2e83c41c340cb91";
  #    sha256 = "sha256-t4oui8CCZU7asE2+4/MhSp1ZwTyKGFBvQ+0Vw5aeqJY=";
  #  })
  #  freecad
  #  ;

  # fava doesn't support beancount v3 yet
  # https://github.com/beancount/fava/issues/1831
  # https://github.com/NixOS/nixpkgs/issues/325945
  #inherit
  #  (nixpkgs {
  #    rev = "59b1aef59071cae6e87859dc65de973d2cc595c0";
  #    sha256 = "sha256-NnvvuMs5ZRJMFxjheq0VPwB8tltv61TwXa/i1Qogik4=";
  #  })
  #  beancount
  #  fava
  #  ;

  #inherit
  #  (nixpkgs {
  #    rev = "f008a5b2616de80a25e705e9019d9b7283faab8b";
  #    sha256 = "sha256-Lnd9hT46PiBxxZHnjOcRaRqUEt9whJLgd/VNd4xqhII=";
  #  })
  #  calibre-web
  #  ;

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
  #inherit
  #  (nixpkgs {
  #    rev = "5a6d31066e324f3631fde06da482e7d47f674005";
  #    sha256 = "";
  #  })
  #  neatvnc
  #  wf-recorder
  #  ;

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
