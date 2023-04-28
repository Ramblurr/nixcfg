{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with types; {
  options = {
    deviceSpecific = {
      vpn = {
        mullvad.enable = mkOption {
          type = bool;
          default = false;
        };
        tailscale.enable = mkOption {
          type = bool;
          default = false;
        };
      };
    };
  };
}
