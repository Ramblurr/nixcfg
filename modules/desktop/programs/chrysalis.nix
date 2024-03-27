{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.chrysalis;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.chrysalis = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    services.udev.packages = [ pkgs.chrysalis ];
    home-manager.users."${username}" = { pkgs, config, ... }@hm: {
      home.packages = [ pkgs.chrysalis ];
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".config/chrysalis" ".config/Chrysalis" ]; };
    };
  };
}
