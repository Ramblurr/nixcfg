{
  lib,
  pkgs,
  config,
  inputs,
  osConfig,
  ...
}:

let
  uid = toString osConfig.users.users.hirde.uid;
  inherit (osConfig.repo.secrets.global) domain;
  forge = "git.${domain.home}";
  models = import ../../modules/dev/llms/models.nix;
  cliproxyProvider = import ../../modules/dev/llms/cliproxy-pi-provider.nix {
    inherit (osConfig.modules.dev.llms.proxy) url;
    apiKey = "!op read 'op://moot/cliproxyapi hirde/credential'";
  };
  workspaceDir = config.home.homeDirectory;
  repoDir = "${workspaceDir}/thingstead";
  pluginsStateDir = "${config.xdg.dataHome}/ol-plugins-sync";
  codexBin = lib.getExe inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.codex;
  op = "${config.my.programs.op-connect.package}/bin/op";
  opCredentials = {
    openai-api-key = "op://moot/openai hirde/credential";
    anthropic-api-key = "op://moot/anthropic api/credential";
    hirde-password = "op://moot/matrix hirde/password";
  };
  staticCredentials = {
    matrix-homeserver = "https://matrix.${domain.moot}";
    hirde-mxid = "@hirde:${domain.moot}";
    lawspeaker-mxid = "@ramblurr:outskirtslabs.com";
  };
  gitCredentialHelper = pkgs.writeShellApplication {
    name = "hirde-git-credential";
    text = ''
      [ "''${1:-}" = get ] || exit 0
      cat > /dev/null
      echo username=hirde
      printf 'password=%s\n' "$(${op} read -n 'op://moot/${forge} hirde/forgejo-access-token')"
    '';
  };
  jdk = pkgs.temurin-bin-27;
  clojure = pkgs.clojure.override { inherit jdk; };
  startHirde = pkgs.writeShellApplication {
    name = "hirde-start";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      set +u
      # shellcheck source=/dev/null
      source /etc/set-environment
      set -u

      # The harness reads credentials as files; keep them on the unit's tmpfs
      # runtime directory, which systemd removes when the service stops.
      umask 077
      export CREDENTIALS_DIRECTORY="$RUNTIME_DIRECTORY"
      ${
        lib.concatStrings (
          lib.mapAttrsToList (name: ref: ''
            ${op} read -n ${lib.escapeShellArg ref} > "$CREDENTIALS_DIRECTORY/${name}"
          '') opCredentials
        )
      }${
        lib.concatStrings (
          lib.mapAttrsToList (name: value: ''
            printf '%s\n' ${lib.escapeShellArg value} > "$CREDENTIALS_DIRECTORY/${name}"
          '') staticCredentials
        )
      }
      OPENAI_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/openai-api-key")"
      ANTHROPIC_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/anthropic-api-key")"
      export OPENAI_API_KEY
      export ANTHROPIC_API_KEY
      export JAVA_HOME="${jdk.home}"
      export JAVA_CMD="${jdk}/bin/java"

      if [ ! -d "${repoDir}/.git" ]; then
        echo "thingstead repo not found at ${repoDir}" >&2
        exit 1
      fi

      cd "${repoDir}"
      eval "$(direnv export bash)"
      exec bb run "$@"
    '';
  };
  bootstrapRepo = pkgs.writeShellApplication {
    name = "hirde-bootstrap-repo";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
    ];
    text = ''
      if [ -d "${repoDir}/.git" ]; then
        exit 0
      fi

      rm -rf "${repoDir}"
      export GIT_TERMINAL_PROMPT=0
      git clone https://${forge}/ramblurr/thingstead.git "${repoDir}"
    '';
  };
in
{
  imports = [
    inputs.sops-nix.homeManagerModules.sops
    ../../modules/dev/llms/pi-nix.nix
    ./home/op-connect.nix
    ./home/t3.nix
  ];
  home.homeDirectory = "/var/lib/wrights/wright-000";
  home.sessionVariables = {
    DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${uid}/bus";
    XDG_RUNTIME_DIR = "/run/user/${uid}";
    XDG_CACHE_HOME = config.xdg.cacheHome;
    XDG_CONFIG_HOME = config.xdg.configHome;
    XDG_DATA_HOME = config.xdg.dataHome;
    XDG_STATE_HOME = config.xdg.stateHome;
    DO_NOT_TRACK = "1";
    NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
    BUN_INSTALL = "$XDG_DATA_HOME/bun";
    BUN_INSTALL_CACHE_DIR = "$XDG_CACHE_HOME/bun";
    PNPM_HOME = "$XDG_DATA_HOME/pnpm";
    YARN_CACHE_FOLDER = "$XDG_CACHE_HOME/yarn";
    YARN_GLOBAL_FOLDER = "$XDG_DATA_HOME/yarn";
  };
  systemd.user.startServices = "sd-switch";
  my.services.t3code = {
    enable = true;
    piAgentDir = "${config.xdg.configHome}/pi-t3/agent";
  };
  my.programs.op-connect = {
    enable = true;
    host = "http://${lib.head osConfig.site.net.mgmt.hosts4.onepassword-connect}:8080";
    tokenFile = config.sops.secrets.onepassword-connect-token.path;
  };
  programs.pi.instances.pi-t3 = {
    settings = config.programs.pi.instances.pi.settings // {
      packages = [ "${pluginsStateDir}/profiles/pi" ];
    };
    models.providers.cliproxy = cliproxyProvider;
  };
  programs.pi.instances.pi.models.providers = {
    cliproxy = cliproxyProvider;
    openai.apiKey = "!op read 'op://moot/openai hirde/credential'";
    openrouter.apiKey = "!op read 'op://moot/openrouter hirde/credential'";
  };
  programs.pi.instances.pi.settings = {
    defaultProvider = "cliproxy";
    defaultModel = models.defaults.pi;
    defaultThinkingLevel = "high";
    packages = [ "${pluginsStateDir}/profiles/pi-cli" ];
    npmCommand = [ "pnpm" ];
    hideThinkingBlock = true;
    theme = "dark";
    steeringMode = "all";
    showCacheMissNotices = true;
    shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
  };
  programs.bash = {
    enable = true;
    initExtra = ''
      [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
    '';
  };
  home.packages = [
    pkgs.age
    pkgs.babashka
    pkgs.bash
    pkgs.brepl
    clojure
    pkgs.coreutils
    pkgs.curl
    pkgs.diffutils
    pkgs.dig
    pkgs.fd
    pkgs.findutils
    pkgs.ghq
    pkgs.git
    pkgs.gnugrep
    pkgs.gnused
    pkgs.harness-plugins-sync
    jdk
    pkgs.jq
    pkgs.killall
    pkgs.lshw
    pkgs.lsof
    pkgs.maven
    pkgs.nixos-rebuild-ng
    pkgs.pnpm
    pkgs.python3
    pkgs.rclone
    pkgs.ripgrep
    pkgs.smartmontools
    pkgs.sops
    pkgs.sudo
    pkgs.tcpdump
    pkgs.tmux
    pkgs.unzip
    pkgs.wget
    pkgs.yq-go
  ];
  sops.defaultSopsFile = ./wright-000.sops.yaml;
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
  sops.secrets.onepassword-connect-token = { };

  systemd.user.services.hirde = {
    Unit = {
      Description = "Hirde wright harness service";
      After = [ "sops-nix.service" ];
      Requires = [ "sops-nix.service" ];
    };
    Service = {
      Type = "simple";
      WorkingDirectory = workspaceDir;
      ExecStartPre = "${bootstrapRepo}/bin/hirde-bootstrap-repo";
      ExecStart = "${startHirde}/bin/hirde-start";
      ExecStopPost = "${pkgs.coreutils}/bin/rm -f ${workspaceDir}/.nrepl-port";
      Restart = "on-failure";
      RestartSec = "5s";
      TimeoutStartSec = "10min";
      RuntimeDirectory = "hirde";
      RuntimeDirectoryMode = "0700";
    };
    Install.WantedBy = [ "default.target" ];
  };

  xdg.configFile."ol-plugins-sync/config.edn".text = ''
    {:state-dir ${builtins.toJSON pluginsStateDir}
     :t3-settings ${builtins.toJSON "${config.xdg.configHome}/t3/userdata/settings.json"}
     :discover-t3 true
     :include-antigravity-global false
     :codex-bin ${builtins.toJSON codexBin}
     :retain-revs 2
     :peers []
     :packs {"harness-plugins" {:url "https://git.outskirtslabs.com/casey/skills.git" :branch "main"}}
     :defaults {:plugins ["my@harness-plugins"]}
     :profiles {"pi-cli" {:harness :pi :dir ${builtins.toJSON "${config.xdg.configHome}/pi/agent"}}}}
  '';

  systemd.user.services.harness-plugins-sync = {
    Unit = {
      Description = "Sync agent plugin packs for hirde";
      After = [ "network-online.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${lib.getExe pkgs.harness-plugins-sync} sync";
    };
  };

  systemd.user.timers.harness-plugins-sync = {
    Unit = {
      Description = "Periodically sync agent plugin packs for hirde";
    };
    Timer = {
      OnBootSec = "1min";
      OnUnitActiveSec = "15min";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };

  programs.git = {
    enable = true;
    settings = {
      user.email = "hirde@${domain.moot}";
      user.name = "Hirde";
      init.defaultBranch = "main";
      pull.rebase = true;
      ghq.root = "~/src";
      safe.directory = repoDir;
      credential."https://${forge}".helper = "${gitCredentialHelper}/bin/hirde-git-credential";
    };
  };
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
    config = {
      warn_timeout = "60s";
      whitelist.exact = [
        repoDir
      ];
    };
  };

  xdg.configFile."pnpm/config.yaml".text = ''
    minimumReleaseAge: 10080
  '';

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  home.stateVersion = "26.05";
}
