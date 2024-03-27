{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.shell.atuin;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.shell.atuin = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = { pkgs, config, ... }@hm: {
      programs.atuin = {
        enable = true;
        enableBashIntegration = true;
        settings = { style = "compact"; };
      };
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".config/atuin" ]; };
      home.file = mkIf withImpermanence {
        ".local/share/atuin".source = config.lib.file.mkOutOfStoreSymlink "/persist/extra/atuin";
      };
    };
  };
}
