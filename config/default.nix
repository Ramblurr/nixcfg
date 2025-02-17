{ inputs, ... }:
{
  imports = [
    ./secrets.nix
    ./attic.nix
    ./site.nix
    ./ramblurr.nix
    ./root.nix
  ];
}
