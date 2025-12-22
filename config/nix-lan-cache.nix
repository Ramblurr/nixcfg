{
  config,
  lib,
  ...
}:
let
  cfg = config.home.nix-lan-cache;
in
{
  options.home.nix-lan-cache = {
    enable = lib.mkEnableOption "Enable local nix cache";
  };
  config = lib.mkIf cfg.enable {
    nix = {
      settings = {
        substituters = [
          config.repo.secrets.global.nixCacheSubstituter
        ];
        trusted-public-keys = [
          config.repo.secrets.global.nixCachePublicKey
        ];
      };
      extraOptions = ''
        # Ensure we can still build when missing-server is not accessible
        fallback = true
      '';
    };
  };
}
