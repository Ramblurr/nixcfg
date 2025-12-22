{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.git;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
in
{
  options.modules.shell.git = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.difftastic.enable = true;
      programs.git = {
        enable = true;
        signing.key = "8865AA3A7BD80355";
        signing.signByDefault = false;
        settings = {
          user.editor = "vim";
          user.email = config.modules.users.primaryUser.email;
          user.name = config.modules.users.primaryUser.name;
          init.defaultBranch = "main";
          pull.rebase = true;
          safe = {
            directory = "${homeDirectory}/src/nixcfg";
          };
          credential.helper = [
            "cache --timeout 21600"
            "${pkgs.git-credential-oauth}/bin/git-credential-oauth"
          ];
          alias = {
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
            lol = ''log --graph --decorate --pretty=oneline --abbrev-commit'';
            lola = ''log --graph --decorate --pretty=oneline --abbrev-commit --all'';
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
        };
      };
    };
  };
}
