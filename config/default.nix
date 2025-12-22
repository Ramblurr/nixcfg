{ ... }:
{
  imports = [
    ./secrets.nix
    ./home-wifi.nix
    ./nix-lan-cache.nix
    ./site.nix
    ./ramblurr.nix
    ./root.nix
  ];
}
