{
  pkgs,
  config,
  ...
}: {
  config = {
    environment.pathsToLink = ["/share/zsh"];

    # BUG: come onnnnn nixpkgs
    programs.zsh.enable = true;
    # users.users.ramblurr.ignoreShellProgramCheck = true;

    home-manager.users.ramblurr = {pkgs, ...} @ hm: {
      programs.fzf = {
        enable = true;
        enableZshIntegration = true;
      };
      home.file = {
        ".config/zsh" = {
          source = ../configs/zsh;
          recursive = true;
        };
      };
      programs.zsh = {
        enable = true;
        autocd = true;
        enableAutosuggestions = true;
        enableCompletion = true;
        enableSyntaxHighlighting = true;
        dotDir = ".config/zsh";
        history = {
          size = 5000000;
          save = 5000000;
          path = "${hm.config.home.homeDirectory}/.zhistory";
          ignoreDups = true;
          ignoreSpace = true;
          expireDuplicatesFirst = true;
          share = true;
          ignorePatterns = [
            "ls"
            "cd"
            "cd -"
            "pwd"
            "exit"
            "date"
            "* --help"
            "man *"
            "zstyle *"
          ];
        };
        shellAliases = {
          "nixcfg" = "cd ${hm.config.home.homeDirectory}/src/nixcfg";
          "reshell!" = "exec $SHELL -l";
          "exa" = "exa --group-directories-first";
          ".." = "cd ..";
          "..." = "cd ../..";
          "...." = "cd ../../../";
          "j" = "z";
          "v" = "vim";
          "vi" = "vim";
          "ga" = "git add";
          "gap" = "git add --patch";
          "gc" = "git commit";
          "gcm" = "git commit -m";
          "gcam" = "git commit --amend";
          "gca" = "git commit --amend --no-edit";
          "gs" = "git status";
          "gd" = "git diff";
          "gf" = "git fetch";
          "gr" = "git rebase";
          "gp" = "git push";
          "gu" = "git unstage";
          "gg" = "git graph";
          "gco" = "git checkout";
          "gcs" = "git commit -S -m";
          "tree" = "tree -CAFa -I \"CVS|*.*.package|.svn|.git|.hg|node_modules|bower_components\" --dirsfirst";
          "zz" = "quit";
          "tf" = "terraform";
          "mkdir" = "mkdir -p";
          "cp" = "cp -r";
          "scp" = "scp -r";
          "ls" = "exa -l --group-directories-first";
          "ll" = "ls -lahF --color=auto --group-directories-first";
          "lsl" = "ls -lhF --color=auto --group-directories-first";
          "utcnow" = "date -u +\"%Y-%m-%d %H:%M:%S\"";
          "task" = "go-task";
          "k" = "kubectl";
        };

        envExtra = ''
          # SESSION
          ${
            ""
            /*
            hm.config.lib.shell.exportAll config.environment.sessionVariables
            */
          }

          # HM SESSION
          ${
            ""
            /*
            hm.config.lib.shell.exportAll hm.config.home.sessionVariables
            */
          }
        '';

        initExtra = ''
          source ~/.config/zsh/init.zsh
        '';

        profileExtra = ''
        '';
      };
    };
  };
}
