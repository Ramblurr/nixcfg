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
      programs.tmux = {
        enable = true;
        historyLimit = 100000;
        resizeAmount = 5;
        escapeTime = 0;
        baseIndex = 1; # Widows numbers begin with 1
        keyMode = "vi";
        newSession = true;
        extraConfig = ''

          set -g default-terminal "tmux-256color"
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

          # switch panes using Alt-arrow without prefix
          bind -n M-Left select-pane -L
          bind -n M-Right select-pane -R
          bind -n M-Up select-pane -U
          bind -n M-Down select-pane -D

          # resize panes more easily
          bind < resize-pane -L 10
          bind > resize-pane -R 10
          bind - resize-pane -D 10
          bind + resize-pane -U 10

          # window separators
          set-option -wg window-status-separator ""

          # monitor window changes
          set-option -wg monitor-activity on
          set-option -wg monitor-bell on

          # set statusbar update interval
          set-option -g status-interval 1
        '';
      };
    };
  };
}
