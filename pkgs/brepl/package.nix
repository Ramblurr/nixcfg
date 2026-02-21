{
  callPackage,
  fetchFromGitHub,
  nix-prefetch-github,
  ast-grep,
  gh,
  pkgs-lib,
  ...
}:
let
  src = fetchFromGitHub {
    owner = "licht1stein";
    repo = "brepl";
    rev = "v2.7.1";
    hash = "sha256-Obv2kSEsgZacY4T3HU1/FqTx4y2dRiCgk9j2tPPd3+o=";
  };
  pkg = callPackage (src + "/package.nix") { };
in
pkg.overrideAttrs (old: {
  passthru = (old.passthru or { }) // {
    updateScript = pkgs-lib.writeUpdateScript {
      packageToUpdate = "brepl";
      utils = [
        nix-prefetch-github
        ast-grep
        gh
      ];
      script = ./update.bb;
    };
  };
})
