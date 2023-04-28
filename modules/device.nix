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
      backup = {
        borgmatic = {
          enable = mkOption {
            type = bool;
            default = false;
          };
          name = mkOption {
            type = str;
          };

          exclude-patterns = mkOption {
            type = listOf str;
            default = [];
          };
          repositories = mkOption {
            type = listOf str;
            description = "Paths to repositories.";
            example =
              literalExpression ''["ssh://myuser@myrepo.myserver.com/./repo"]'';
          };
        };
      };
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
