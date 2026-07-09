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
    myhm = {
      home.packages = with pkgs; [ pyright ];
      xdg.configFile."uv/uv.toml".text = ''
        exclude-newer = "7 days"
      '';
    };
  };
}
