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
    rev = "v2.6.3";
    hash = "sha256-1r+7DQcfOSD9gaBE3Hu961Se5lUqxIHPzF4E2NaNl/E=";
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
