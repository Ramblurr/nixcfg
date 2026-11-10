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
  credentialCacheSocket = "${homeDirectory}/.cache/git/credential-systemd/socket";
  credentialCacheHelper = pkgs.writeShellScript "git-credential-cache-systemd" ''
    set -eu
    ${pkgs.systemd}/bin/systemctl --user start git-credential-cache.service
    exec ${pkgs.git}/bin/git credential-cache \
      --socket=${lib.escapeShellArg credentialCacheSocket} --timeout=86400 "$@"
  '';
in
{
  options.modules.shell.git = {
    enable = lib.mkEnableOption "";
    credentialCache.enable = lib.mkEnableOption "the systemd-managed Git credential cache" // {
      default = true;
    };
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      home.packages = with pkgs; [
        forgejo-cli
      ];
      systemd.user.services.git-credential-cache = lib.mkIf cfg.credentialCache.enable {
        Unit.Description = "In-memory Git credential cache";
        Service = {
          Type = "exec";
          ExecStart = "${pkgs.git}/bin/git credential-cache--daemon --debug ${credentialCacheSocket}";
          ExecStartPost = pkgs.writeShellScript "git-credential-cache-ready" ''
            while [ ! -S ${lib.escapeShellArg credentialCacheSocket} ]; do
              ${pkgs.coreutils}/bin/sleep 0.05
            done
          '';
          TimeoutStartSec = 10;
          # Git exits when empty; systemd must own the next daemon too.
          Restart = "always";
          RestartSec = 0;
          CacheDirectory = "git/credential-systemd";
          CacheDirectoryMode = "0700";
          UMask = "0077";
        };
        Install.WantedBy = [ "default.target" ];
      };
      programs.difftastic.enable = true;
      programs.difftastic.git.enable = true;
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
          ghq.root = "~/src";
          safe = {
            directory = "${homeDirectory}/src/nixcfg";
          };
          credential.helper = [
            (if cfg.credentialCache.enable then "${credentialCacheHelper}" else "cache --timeout 86400")
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
            lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
            lola = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
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
            # ref https://piechowski.io/post/git-commands-before-reading-code/
            who = "shortlog -sn --no-merges HEAD";
            dmg = "log --oneline -i -E --grep='(incident|outage|downtime|rollback|revert|mitigate|mitigation|hotfix|broke|prod)' --since='1 year ago'";
            bugs = "log --oneline -i -E --grep='(bug|bugfix|fix|fixed|fixes|defect|regression|hotfix|broke)' --since='1 year ago'";
            bugfiles = "!git log --name-only --format='' -i -E --grep='(bug|bugfix|fix|fixed|fixes|defect|regression|hotfix|broke)' --since='1 year ago' | sort | uniq -c | sort -nr";
            monthly = "!git log --since='1 year ago' --format='%ad' --date=format:'%Y-%m' | sort | uniq -c";
            churn = "!git log --format='' --name-only --diff-filter=AM --since='1 year ago' | sort | uniq -c | sort -nr | head -20";
          };
        };
        ignores = [
          ".tmuxb_session"
          "prompts"
          "archive"
          ".worktrees"
          "extra"
          ".direnv"
          ".scratch"
          ".scratch-org"
        ];
      };
    };
  };
}
