{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.direnv;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.direnv = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      package = pkgs.direnv;
      nix-direnv.enable = true;
    };
    # Nix options for derivations to persist garbage collection
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    environment.pathsToLink = [ "/share/nix-direnv" ];

    myhm = {
      persistence = mkIf withImpermanence { directories = [ ".local/share/direnv" ]; };
    };
  };
}
