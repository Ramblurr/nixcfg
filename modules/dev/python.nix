{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.python;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.python = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      uv
      python3Packages.python-lsp-server
      python3Packages.netaddr
      (python3.withPackages (
        ps: with ps; [
          pip
          pytest
          virtualenv
          black
          isort
          setuptools
          wheel
          requests
          netaddr
        ]
      ))
    ];
    home-manager.users."${username}" = {
      home.packages = with pkgs; [ pyright ];
      home.persistence."/persist" = mkIf withImpermanence {
        directories = [ ".cache/pypoetry/virtualenvs/" ];
      };
    };
  };
}
