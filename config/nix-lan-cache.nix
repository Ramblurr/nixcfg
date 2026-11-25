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
    substituter = lib.mkOption {
      type = lib.types.str;
      default = config.repo.secrets.global.nixCacheSubstituter;
      defaultText = lib.literalExpression "config.repo.secrets.global.nixCacheSubstituter";
      description = "URL of mali's ncps for this host.";
    };
  };
  config = lib.mkIf cfg.enable {
    nix = {
      settings = {
        extra-substituters = [ cfg.substituter ];
        extra-trusted-public-keys = [
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
