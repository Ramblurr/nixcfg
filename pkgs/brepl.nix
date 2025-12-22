{
  pkgs,
  callPackage,
  fetchFromGitHub,
  ...
}:
callPackage (
  fetchFromGitHub {
    owner = "licht1stein";
    repo = "brepl";
    rev = "v2.3.1";
    hash = "sha256-zNzQ2JTYWwaWJ5inJa2B8WHu3CFM8CzqkWuB/Ekr7lw=";
  }
  + "/package.nix"
) { }
