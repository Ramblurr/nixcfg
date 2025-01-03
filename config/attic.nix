{
  pkgs,
  config,
  globals,
  lib,
  ...
}:
let
  cfg = config.home.attic;
in
{
  options.home.attic = {
    enable = lib.mkEnableOption "Enable local attic substituters";
  };
  config = lib.mkIf cfg.enable {
    #nix.settings = {
    #  extra-substituters = [
    #    globals.localAtticSubstituter
    #  ];
    #  extra-trusted-public-keys = [
    #    globals.localAtticPublicKey
    #  ];
    #};
  };
}
