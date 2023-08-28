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
  cfg = config.modules.shell.git;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.shell.git = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.git = {
        enable = true;
        signing.key = config.modules.users.primaryUser.signingKey;
        signing.signByDefault = false;
        userEmail = config.modules.users.primaryUser.email;
        userName = config.modules.users.primaryUser.name;
        extraConfig = {
          init.defaultBranch = "main";
          user.editor = "vim";
          pull.rebase = true;
          safe = {
            directory = "${homeDirectory}/src/nixcfg";
          };
        };
        aliases = {
          ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]\" --decorate --numstat";
        };

        difftastic = {
          enable = true;
        };
      };
    };
  };
}
