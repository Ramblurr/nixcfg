{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.node;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.node = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".cache/bun"
          ".cache/node"
          ".cache/npm"
          ".cache/yarn"
          ".config/npm"
          ".config/yarn"
          ".local/share/bun"
          ".local/share/npm"
          ".local/share/pnpm"
          ".local/share/volta"
          ".local/share/yarn"
        ];
      };
    };

    myhm = _: {
      home.packages = with pkgs; [
        nodejs_24
        yarn
        bun
        deno
      ];
      home.sessionVariables = {
        # https://consoledonottrack.com/
        # https://bun.sh/docs/runtime/bunfig#telemetry
        DO_NOT_TRACK = "1";
        NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
        NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
        NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
        NPM_CONFIG_LOGS_DIR = "$XDG_STATE_HOME/npm/logs";
        NPM_CONFIG_UPDATE_NOTIFIER = "false"; # Prevents ~/.npm/_update-notifier-last-checked
        NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
        NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
        BUN_INSTALL = "$XDG_DATA_HOME/bun";
        BUN_INSTALL_CACHE_DIR = "$XDG_CACHE_HOME/bun";
        PNPM_HOME = "$XDG_DATA_HOME/pnpm";
        YARN_CACHE_FOLDER = "$XDG_CACHE_HOME/yarn";
        YARN_GLOBAL_FOLDER = "$XDG_DATA_HOME/yarn";
        VOLTA_HOME = "$XDG_DATA_HOME/volta";
      };
      xdg.configFile."npm/config".text = ''
        cache=''${XDG_CACHE_HOME}/npm
        prefix=''${XDG_DATA_HOME}/npm
        tmp=''${XDG_RUNTIME_DIR}/npm
        init-module=''${XDG_CONFIG_DIR}/npm/config/npm-init.js
        min-release-age=7
        ignore-scripts=true
      '';

      xdg.configFile."pnpm/rc".text = ''
        minimum-release-age=10080
      '';

      xdg.configFile.".bunfig.toml".text = ''
        [install]
        minimumReleaseAge = 604800
      '';
      home.file = lib.mkMerge [
        (lib.mkIf pkgs.stdenv.isDarwin {
          "Library/Preferences/pnpm/rc".text = ''
            minimum-release-age=10080 # minutes
          '';
        })
      ];
    };
  };
}
