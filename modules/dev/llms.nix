{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
  llm-agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  cat-url-markdown = pkgs.writeShellScriptBin "cat-url-markdown" ''
    if [ -z "$1" ]; then
      echo "usage: $(basename "$0") URL [FILENAME]"
      exit 1
    fi
    curl -sSL --output - $(printf "https://r.jina.ai/%s" $1)
  '';
  cat-url-markdown-auto = pkgs.writeShellApplication {
    name = "cat-url-markdown-auto";
    runtimeInputs = [
      pkgs.babashka
      cat-url-markdown
      pkgs.curl
    ];
    text = ''
      exec bb ${../../scripts/cat_url_markdown_auto.clj} "$@"
    '';
  };
in
{
  imports = [
    ./llms/agents.nix
    ./llms/clients.nix
    ./llms/pi.nix
    ./llms/plugins-sync.nix
    ./llms/profile.nix
    ./llms/t3.nix
  ];
  options.modules.dev.llms = {
    enable = lib.mkEnableOption "";
    ollama.enable = lib.mkEnableOption "";
    desktop.enable = lib.mkEnableOption "graphical agent tools";
    onepasswordConnect = {
      enable = lib.mkEnableOption "agent key reads through 1Password Connect, using the host-local Connect token file";
      tokenFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default =
          if cfg.onepasswordConnect.enable then
            config.modules.services.onepassword-systemd-credentials.bootstrapTokenFile
          else
            null;
        defaultText = lib.literalExpression "config.modules.services.onepassword-systemd-credentials.bootstrapTokenFile when enable is set, else null";
        description = ''
          Runtime path to a 1Password Connect token that can read the LLM vault.
          When set, agent API-key reads use Connect instead of the desktop-approved `op` session.
        '';
      };
      host = lib.mkOption {
        type = lib.types.str;
        default = "http://${builtins.head config.site.net.mgmt.hosts4.onepassword-connect}:8080";
        description = "1Password Connect endpoint used with `tokenFile`.";
      };
      package = lib.mkOption {
        type = lib.types.package;
        readOnly = true;
        default = pkgs.writeShellScriptBin "op-read-llm" ''
          OP_CONNECT_HOST=${lib.escapeShellArg cfg.onepasswordConnect.host} \
          OP_CONNECT_TOKEN="$(< ${lib.escapeShellArg cfg.onepasswordConnect.tokenFile})" \
            exec ${lib.getExe pkgs._1password-cli} read "$@"
        '';
        defaultText = lib.literalMD "`op-read-llm`, built from `host` and `tokenFile`";
        description = "`op-read-llm`, which runs `op read` through Connect with `tokenFile`.";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.ollama = lib.mkIf cfg.ollama.enable {
      enable = true;
      package = pkgs.ollama-cuda;
    };
    services.open-webui.enable = false;
    services.open-webui.port = 11180;
    myhm = {
      home.packages =
        with pkgs;
        [
          llm-agents.swamp
          geckodriver
          chromedriver
          spel
          mcp-inspector
          cat-url-markdown
          cat-url-markdown-auto
          inputs.tmux-buddy.packages.${pkgs.stdenv.hostPlatform.system}.default
          llm-agents.plannotator
          llm-agents.plannotator-tui
          pkgs.hindsight-cli
          pkgs.ucp-cli
          pkgs.link-cli
          inputs.git-lines.packages.${pkgs.stdenv.hostPlatform.system}.default
          universal-ctags
          difftastic
          ast-grep
          nushell
        ]
        ++ lib.optionals cfg.desktop.enable [
          pkgs.glimpseui
          pkgs.dotool
          pkgs.wtype
        ]
        ++ lib.optionals cfg.ollama.enable [
          ollama-cuda
        ];
    };
  };
}
