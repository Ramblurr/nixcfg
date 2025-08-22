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
  cfg = config.modules.shell.zsh;
  withImpermanence = config.modules.impermanence.enable;

  functions = ''
    function generate-password() { strings /dev/urandom | grep -o '[[:alnum:]]' | head -n ''${1:-32} | tr -d '\n'; echo }
    function where-from() { readlink -f $(which $1) }
  '';
in
{
  options.modules.shell.zsh = {
    enable = lib.mkEnableOption "";
    starship.enable = lib.mkEnableOption "";
    powerlevel10k.enable = lib.mkEnableOption "";
    powerlevel10k.configFile = lib.mkOption {
      type = lib.types.path;
      default = ./configs/p10k-config;
      description = "Path to the Powerlevel10k configuration file.";
    };
    profileExtra = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = "";
    };
  };
  config = mkIf cfg.enable {
    environment.pathsToLink = [ "/share/zsh" ];

    programs.zsh.enable = true;
    myhm =
      { ... }@hm:
      {
        home.packages = with pkgs; [
          eza
          ripgrep
        ];
        programs.fzf = {
          enable = true;
          enableZshIntegration = true;
        };
        home.file = {
          ".config/zsh" = {
            source = ./configs;
            recursive = true;
          };
        };
        programs.starship = {
          enable = cfg.starship.enable;
          enableBashIntegration = false;
          enableZshIntegration = true;
          settings = {
            git_status = {
              ignore_submodules = true;
              untracked = "";
            };
          };
        };
        # NOTE: I am keeping zsh's history in a directory, and persisting that directory
        # Before I tried just keeping the histfile in the normal location and persisting
        # the file, but often the symlink would be overriden causing home-manager activation errors.
        home.persistence."/persist${hm.config.home.homeDirectory}" = mkIf withImpermanence {
          allowOther = true;
          directories = [
            {
              method = "symlink";
              directory = ".local/state/zsh";
            }
          ];
        };
        programs.zsh = {
          enable = true;
          autocd = true;
          autosuggestion.enable = true;
          enableCompletion = true;
          syntaxHighlighting.enable = true;
          dotDir = "${hm.config.xdg.configHome}/zsh";
          history = {
            size = 5000000;
            save = 5000000;
            path = "${hm.config.xdg.stateHome}/zsh/zhistory";
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
          plugins = mkIf cfg.powerlevel10k.enable [
            {
              name = "powerlevel10k-config";
              src = lib.cleanSource cfg.powerlevel10k.configFile;
              file = "p10k.zsh";
            }
          ];
          shellAliases = {
            "md" = "glow -p";
            "borgmatic" =
              "sudo systemd-run --pty --property EnvironmentFile=/run/secrets/borgmatic-env borgmatic";
            "mvm" = ''mvn -gs "$XDG_CONFIG_HOME"/maven/settings.xml'';
            "open" = "re.sonny.Junction";
            "nixcfg" = "cd ${hm.config.home.homeDirectory}/nixcfg";
            "reshell!" = "exec $SHELL -l";
            "eza" = "eza --group-directories-first";
            ".." = "cd ..";
            "..." = "cd ../..";
            "...." = "cd ../../../";
            "j" = "z";
            "v" = "vim";
            #"vi" = "emasclient -nw";
            #"vim" = "emacsclient -nw";
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
            "tree" =
              ''tree -CAFa -I "CVS|*.*.package|.svn|.git|.hg|node_modules|bower_components" --dirsfirst'';
            "zz" = "quit";
            "tf" = "tofu";
            "mkdir" = "mkdir -p";
            "cp" = "cp -r";
            "scp" = "scp -r";
            "ls" = "eza -l --group-directories-first";
            "ll" = "ls -lahF --color=auto --group-directories-first";
            "lsl" = "ls -lhF --color=auto --group-directories-first";
            "utcnow" = ''date -u +"%Y-%m-%d %H:%M:%S"'';
            "task" = "go-task";
            "k" = "kubectl";
            rsync = "rsync --info=progress2";
            # systemd
            log = "sudo journalctl --output cat -u"; # log sshd -f
            logu = "journalctl --user --output cat -u"; # log sshd -f
            log-previous-boot = "sudo journalctl --boot=-1";
            st = "sudo systemctl status";
            ss = "sudo systemctl stop";
            sr = "sudo systemctl reload";
            sR = "sudo systemctl restart";
            stu = "systemctl status --user";
            ssu = "systemctl stop --user";
            sru = "systemctl reload --user";
            sRu = "systemctl restart --user";
            list-units = "systemctl list-units --type=service | awk '/.service/ {print $1}'";
          };

          #${hm.config.lib.shell.exportAll config.environment.sessionVariables}
          envExtra = ''
            # SESSION

            # HM SESSION
            ${hm.config.lib.shell.exportAll hm.config.home.sessionVariables}
          '';

          profileExtra = cfg.profileExtra;
        }
        // (
          let
            initContent =
              (
                if cfg.powerlevel10k.enable then
                  ''
                    if [[ "$TERM" != 'dumb' && -z "$INSIDE_EMACS" ]]; then
                      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
                      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
                    fi
                  ''
                else
                  ""
              )
              + ''
                source ~/.config/zsh/init.zsh
                ${functions}
              '';
          in
          if builtins.hasAttr "initContent" hm.options.programs.zsh then
            { initContent = initContent; }
          else
            { initExtra = initContent; }
        );
      };
  };
}
