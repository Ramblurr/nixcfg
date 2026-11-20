{ ... }:
{
  imports = [
    ./secrets.nix
    ./home-wifi.nix
    ./nix-lan-cache.nix
    ./nix-remote-builds.nix
    ./site.nix
    ./root.nix
  ];
}
