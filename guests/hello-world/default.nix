{ pkgs, ... }:

{
  system.stateVersion = "24.11";
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
