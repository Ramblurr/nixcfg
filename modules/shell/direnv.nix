{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.shell.direnv;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.shell.direnv = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      package = pkgs.direnv;
    };
    # Nix options for derivations to persist garbage collection
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    environment.pathsToLink = [ "/share/nix-direnv" ];

    myhm = { persistence = mkIf withImpermanence { directories = [ ".local/share/direnv" ]; }; };
  };
}
