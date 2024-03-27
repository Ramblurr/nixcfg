{ config, inputs, options, lib, pkgs, my, ... }:
with lib;
with lib.my;
let
  devCfg = config.modules.dev;
  cfg = devCfg.fennel;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  nur = import inputs.nur {
    nurpkgs = pkgs;
    inherit pkgs;
  };
in {
  options.modules.dev.fennel = { enable = mkBoolOpt false; };

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
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".config/fennel" ]; };
    };
  };
}
