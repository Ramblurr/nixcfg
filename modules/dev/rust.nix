{
  config,
  lib,
  pkgs,
  ...
}:
let
  devCfg = config.modules.dev;
  cfg = devCfg.rust;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.rust = {
    enable = lib.mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".local/share/rusthome"
          ".local/share/cargo"
        ];
      };
    };

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
