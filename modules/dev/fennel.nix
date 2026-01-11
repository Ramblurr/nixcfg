{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.fennel;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
  nur = import inputs.nur {
    nurpkgs = pkgs;
    inherit pkgs;
  };
in
{
  options.modules.dev.fennel = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lua
      luajit
      lua-language-server
      fennel
      fnlfmt
      nur.repos.bandithedoge.fennel-language-server
      love
    ];
    home-manager.users."${username}" = {
      #home.packages = with pkgs; [
      #];
      home.persistence."/persist" = mkIf withImpermanence {
        directories = [ ".config/fennel" ];
      };
    };
  };
}
