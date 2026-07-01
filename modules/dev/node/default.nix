{
  config,
  lib,
  pkgs,
  ...
}:
let
  devCfg = config.modules.dev;
  cfg = devCfg.node;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
  inherit (pkgs) nodejs-slim;
  inherit (pkgs) nodejs;
  pnpm = pkgs.pnpm.override { inherit nodejs-slim; };
  yarn = pkgs.yarn.override { inherit nodejs; };
  npmUnavailable = pkgs.writeShellScriptBin "npm" ''
    echo "npm not available use pnpm instead"
    exit 1
  '';
in
{
  options.modules.dev.node = {
    enable = lib.mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    myhm = _: {
      home.packages = [
        (lib.hiPrio npmUnavailable)
        nodejs-slim
        pnpm
        yarn
        pkgs.bun
        pkgs.deno
      ];
      home.sessionVariables = {
        # https://consoledonottrack.com/
        # https://bun.sh/docs/runtime/bunfig#telemetry
        DO_NOT_TRACK = "1";
        NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
        BUN_INSTALL = "$XDG_DATA_HOME/bun";
        BUN_INSTALL_CACHE_DIR = "$XDG_CACHE_HOME/bun";
        PNPM_HOME = "$XDG_DATA_HOME/pnpm";
        YARN_CACHE_FOLDER = "$XDG_CACHE_HOME/yarn";
        YARN_GLOBAL_FOLDER = "$XDG_DATA_HOME/yarn";
        VOLTA_HOME = "$XDG_DATA_HOME/volta";
      };
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
