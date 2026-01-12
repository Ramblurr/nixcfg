{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.handy;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.handy = {
    enable = lib.mkEnableOption "";
    autostart.enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/com.pais.handy"
          ".local/share/com.pais.handy"
        ];
      };
    };
    myhm = {
      home.file.".config/autostart/handy.desktop" = lib.mkIf cfg.autostart.enable (
        lib.my.autostart "handy"
      );
      home.packages = [ inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.handy ];
    };
  };
}
