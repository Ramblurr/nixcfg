{
  options,
  config,
  lib,
  pkgs,
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
    myhm = {...} @ hm: {
      home.packages = with pkgs; [
        logseq
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
