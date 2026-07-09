{
  config,
  lib,
  pkgs,
  ...
}:
let
  devCfg = config.modules.dev;
  cfg = devCfg.rust;
in
{
  options.modules.dev.rust = {
    enable = lib.mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    myhm = _: {
      home.sessionVariables = {
        RUSTUP_HOME = "$XDG_DATA_HOME/rusthome";
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
      };
      home.packages = with pkgs; [
        rustup
      ];
    };
  };
}
