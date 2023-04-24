{ config, pkgs, ... }:

{
  virtualisation {
    podman = {
      enable = true;
      defaultNetwork.dnsname.enable = true;
      extraPackages = [ pkgs.zfs ];
    }
  }
}
