{ config, options, lib, pkgs, my, ... }:
with lib;
with lib.my;
let
  devCfg = config.modules.dev;
  cfg = devCfg.python;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.python = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      python311Packages.netaddr
      (python311.withPackages (ps:
        with ps; [
          pip
          pytest
          virtualenv
          black
          python-lsp-black
          setuptools
          wheel
          requests
          netaddr
        ]))
    ];
    home-manager.users."${username}" = {
      home.packages = with pkgs; [ nodePackages_latest.pyright ];
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".cache/pypoetry/virtualenvs/" ]; };
    };
  };
}
