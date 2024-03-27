{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.junction;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.junction = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = { pkgs, config, ... }@hm: {
      home.packages = [ pkgs.junction ];
    };
  };
}
