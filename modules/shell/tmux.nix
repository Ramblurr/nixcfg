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
  cfg = config.modules.shell.tmux;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.tmux = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.tmux =
        let
          accent = "#{@main_accent}";
          client_prefix =
            let
              left = "#[noreverse]#{?client_prefix,,}";
              right = "#[noreverse]#{?client_prefix, ,}";
              icon = "#[reverse]#{?client_prefix,,}";
            in
            "#[fg=${accent}]${left}${icon}${right}";
          current_window =
            let
              bracket = "#[bold,fg=${accent}]";
              name = "#[bold,fg=default]#W";
            in
            "${bracket}#I[${name}${bracket}] ";

          window_status =
            let
              bracket = "#[bold,fg=white]";
              index = "#[bold,fg=default]#I";
              name = "#[nobold,fg=default]#W";
            in
            "${index}${bracket}[${name}${bracket}] ";
          pwd =
            let
              icon = "#[fg=${accent}] ";
              format = "#[fg=default]#{b:pane_current_path}";
            in
            "${icon}${format}";

          git =
            let
              script =
                pkgs.writers.writeNu "git"
                  # nu
                  ''
                    def main [dir: string] {
                        let branch = git -C $dir rev-parse --abbrev-ref HEAD | complete
                        if ($branch.exit_code) == 0 {
                            $"#[fg=orange] ($branch.stdout | str trim)"
                        }
                    }
                  '';
            in
            "#(${script} #{pane_current_path})";
        in
        {
          enable = true;
          secureSocket = true;
          terminal = "tmux-256color";
          historyLimit = 100000;
          resizeAmount = 5;
          escapeTime = 0;
          baseIndex = 1; # Widows numbers begin with 1
          keyMode = "vi";
          newSession = true;
          mouse = true;
          plugins = [ pkgs.tmuxPlugins.gruvbox ];
          extraConfig = ''
            set -g @tmux-gruvbox 'dark'
            set -ga terminal-overrides ",xterm-256color:Tc"
            # use <prefix> s for horizontal split
            bind s split-window -v
            # use <prefix> v for vertical split
            bind v split-window -h
            unbind '"'
            unbind %

            # navigate pans like vim
            bind h select-pane -L
            bind j select-pane -D
            bind k select-pane -U
            bind l select-pane -R

            # navigate windows with Alt-N
            bind-key -n M-1 select-window -t 1
            bind-key -n M-2 select-window -t 2
            bind-key -n M-3 select-window -t 3
            bind-key -n M-4 select-window -t 4

            # fallback where terminals use Alt-N for tab navigations
            bind-key -n M-F1 select-window -t 1
            bind-key -n M-F2 select-window -t 2
            bind-key -n M-F3 select-window -t 3
            bind-key -n M-F4 select-window -t 4

            # window separators
            set-option -wg window-status-separator ""

            # monitor window changes
            set-option -wg monitor-activity on
            set-option -wg monitor-bell on

            # set statusbar update interval
            set-option -g status-interval 1

            #set-option -g @main_accent "orange"
            #set-option -g status-right-length 100
            set-option -g status-left "${client_prefix}"
            #set-option -g status-right "#(whoami)@#(hostname) ${git} ${pwd} -%m-%d %H:%M"
            #set-option -g window-status-current-format "${current_window}"
            #set-option -g window-status-format "${window_status}"
            #set-option -g window-status-separator ""
          '';
        };
    };
  };
}
