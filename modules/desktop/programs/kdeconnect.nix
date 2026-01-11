{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.kdeconnect;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.kdeconnect = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPortRanges = [
      {
        from = 1714;
        to = 1764;
      }
    ];
    networking.firewall.allowedUDPPortRanges = [
      {
        from = 1714;
        to = 1764;
      }
    ];
    home-manager.users."${username}" = _: {
      services.kdeconnect = {
        enable = true;
        indicator = true;
      };
      home.persistence."/persist" = mkIf withImpermanence {
        directories = [ ".config/kdeconnect" ];
      };
    };
  };
}
