{
  options,
  config,
  lib,
  pkgs,
  edge,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.programs.logseq;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.logseq = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.packages = [
        edge.logseq
        #(edge.logseq.override {electron_27 = pkgs.electron_28;})
      ];
      persistence = mkIf withImpermanence {
        directories = [
          ".config/Logseq"
          ".logseq"
        ];
      };
    };
  };
}
