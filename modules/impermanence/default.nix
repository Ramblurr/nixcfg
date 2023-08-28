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
  cfg = config.modules.impermanence;
  username = config.modules.users.primaryUser.username;
in {
  options.modules.impermanence = {
    enable = mkBoolOpt false;
  };
  config =
    mkIf cfg.enable {
    };
}
