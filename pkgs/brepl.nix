{
  callPackage,
  fetchFromGitHub,
  ...
}:
callPackage (
  fetchFromGitHub {
    owner = "licht1stein";
    repo = "brepl";
    rev = "v2.5.1";
    hash = "sha256-5rv7fFRe32a1JL2BAD3mMP2b3VGtiMC+MDphhJgJkgk=";
  }
  + "/package.nix"
) { }
