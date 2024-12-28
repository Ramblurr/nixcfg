{
  pkgs,
  config,
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
    nix.settings = {
      extra-substituters = [ "https://attic.mgmt.${config.repo.secrets.global.domain.home}/socozy" ];
      extra-trusted-public-keys = [
        "socozy:6DGMWTIQnpp/tsHzx45lX1lUOn4oiDwg7WX1/pJASwE="
      ];
    };
  };
}
