{ inputs, ... }:
{
  imports = [
    ./secrets.nix
    ./nix-lan-cache.nix
    ./site.nix
    ./ramblurr.nix
    ./root.nix
  ];
}
