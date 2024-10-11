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

  inherit
    (nixpkgs {
      rev = "26ba28ed1d7126641c6e953bd9516fef0207bb10";
      sha256 = "sha256-2UW72Ioc9Eyv7AmIA26ps2IzoKa6gkpUXbKJB+tRJ4k=";
    })
    go-task
    ;
}
