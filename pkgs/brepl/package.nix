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
    rev = "v2.7.0";
    hash = "sha256-eTQS5LvAOLvx46YS6V/2+bYkWSELTTwZ1ir77eLk99M=";
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
