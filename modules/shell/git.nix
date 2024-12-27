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
  cfg = config.modules.shell.git;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.git = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.git = {
        enable = true;
        signing.key = "8865AA3A7BD80355";
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
          credential.helper = [
            "cache --timeout 21600"
            "${pkgs.git-credential-oauth}/bin/git-credential-oauth"
          ];
        };
        aliases = {
          s = "status -s";
          co = "checkout";
          cob = "checkout -b";
          la = "!git config -l | grep alias | cut -c 7-";
          d = "diff";
          ds = "diff --stat";
          dc = "diff --cached";
          a = "add";
          ap = "add -p";
          c = "commit --verbose";
          ca = "commit -a --verbose";
          cm = "commit -m";
          cam = "commit -a -m";
          m = "commit --amend --verbose";
          # one-line log
          l = ''log --pretty=format:"%C(yellow)%h\ %ad%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate --date=short'';
          rao = "remote add origin";
          ls = ''log --pretty=format:"%C(yellow)%h%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate'';
          ll = ''log --pretty=format:"%C(yellow)%h%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate --numstat'';
          # no colors, for piping
          lnc = ''log --pretty=format:"%h\ %s\ [%cn]"'';
          lds = ''log --pretty=format:"%C(yellow)%h\ %ad%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate --date=short'';
          filelog = "log -u";
          fl = "log -u";
          # show modified files in last commit:
          dl = "!git ll -1";
          # show a diff last commit:
          dlc = "diff --cached HEAD^";
          # find a file path in codebase:
          f = "!git ls-files | grep -i";
          logtree = "log --graph --oneline --decorate --all";
        };

        difftastic = {
          enable = true;
        };
      };
    };
  };
}
