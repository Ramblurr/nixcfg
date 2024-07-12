{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.impermanence;
  username = config.modules.users.primaryUser.username;
in
{
  options.modules.impermanence = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable { };
}
