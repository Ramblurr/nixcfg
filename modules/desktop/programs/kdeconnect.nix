{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.kdeconnect;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.kdeconnect = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPortRanges = [{
      from = 1714;
      to = 1764;
    }];
    networking.firewall.allowedUDPPortRanges = [{
      from = 1714;
      to = 1764;
    }];
    home-manager.users."${username}" = { pkgs, config, ... }@hm: {
      services.kdeconnect = {
        enable = true;
        indicator = true;
      };
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".config/kdeconnect" ]; };
    };
  };
}
