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
    nix = {
      settings = {
        extra-substituters = [
          config.repo.secrets.global.localAtticSubstituter
        ];
        extra-trusted-public-keys = [
          config.repo.secrets.global.localAtticPublicKey
        ];
      };
      extraOptions = ''
        # Ensure we can still build when missing-server is not accessible
        fallback = true
      '';
    };
  };
}
