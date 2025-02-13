{ pkgs, ... }:

{
  system.stateVersion = "24.11";
  sops.defaultSopsFile = ./secrets.sops.yaml;
  networking.hostName = "hello-world";

  services = {
    nginx = {
      enable = true;
      virtualHosts."hello-world.socozy.casa" = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };
}
