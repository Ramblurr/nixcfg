{ pkgs, ... }:

{
  system.stateVersion = "24.11";
  services = {
    nginx = {
      enable = true;
      virtualHosts."hello-world.REDACTED.REDACTED" = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };
}
