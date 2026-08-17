{ pkgs, ... }:
let
  unifi = pkgs.unifi.overrideAttrs (_oldAttrs: rec {
    version = "10.5.67";
    src = pkgs.fetchurl {
      url = "https://dl.ui.com/unifi/${version}/unifi_sysvinit_all.deb";
      hash = "sha256-7juZuVZ4vk4di1jjgm6JEVMbErMu6L8epLSft+dvl4M=";
    };
  });
in
{
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/unifi".mountpoint = "/var/lib/unifi";
  };

  services.unifi = {
    enable = true;
    unifiPackage = unifi;
    mongodbPackage = pkgs.mongodb-7_0;
    # Addams uses the custom nftables firewall in ./modules/firewall.
    openFirewall = false;
  };
}
