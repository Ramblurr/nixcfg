{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-guest.mootWright;
  inherit (cfg) name;
  inherit (config.repo.secrets.global.domain) tailnet;
  vm = config.modules.microvm-guest.mootVm;
  agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  models = import ../dev/llms/models.nix;
  json = pkgs.formats.json { };

  uid = 2001;
  home = "/home/${name}";
  configHome = "${home}/.config";
  proxyAddress = "${vm.network.hostAddress}:${toString vm.network.proxyPort}";

  pluginsStateDir = "${home}/.local/share/ol-plugins-sync";
  t3Home = "${home}/.t3";
  dirs = {
    claude = "${configHome}/claude";
    codex = "${configHome}/codex";
    pi = "${configHome}/pi/agent";
    piT3 = "${configHome}/pi-t3/agent";
  };

  # The egress signs decrypted flows with its own CA. Node keeps its own
  # roots, so it needs the CA named on top of the system bundle.
  egressCa = ../../config/ts-egress-ca.pem;
  caEnv = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    NODE_EXTRA_CA_CERTS = "${egressCa}";
  };
  wrightEnv =
    vm.proxyEnv
    // caEnv
    // lib.genAttrs cfg.credentials (n: "TS_EGRESS_PLACEHOLDER_${n}")
    // {
      # Node's fetch ignores the proxy variables without this, and tailnet
      # names resolve only through the proxy.
      NODE_USE_ENV_PROXY = "1";
      XDG_RUNTIME_DIR = "/run/user/${toString uid}";
      DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString uid}/bus";
      MOOT_MATRIX_ID = cfg.identity.matrixId;
      CLAUDE_CONFIG_DIR = dirs.claude;
      CODEX_HOME = dirs.codex;
      PI_CODING_AGENT_DIR = dirs.pi;
      T3CODE_HOME = t3Home;
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
      DISABLE_AUTOUPDATER = "1";
      DISABLE_TELEMETRY = "1";
    };
  exports =
    env:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg v}") env);

  withPlaceholder =
    bin: package: variable: extraEnv:
    pkgs.writeShellScriptBin bin ''
      ${exports ({ ${variable} = "tailnet"; } // extraEnv)}
      exec ${lib.getExe package} "$@"
    '';
  claude = withPlaceholder "claude" agents.claude-code "ANTHROPIC_AUTH_TOKEN" {
    ANTHROPIC_BASE_URL = cfg.cliproxy.url;
  };
  codex = withPlaceholder "codex" agents.codex "CLIPROXY_API_KEY" { };
  pi = agents.pi.override { useBun = false; };
  antigravity = agents.antigravity-cli;
  t3 = pkgs.t3code.override {
    providerPackages = [
      claude
      codex
      pi
    ];
  };
  providers = [
    claude
    codex
    pi
    antigravity
  ];

  claudeSettings = {
    model = models.defaults.claude;
    includeCoAuthoredBy = false;
    # Claude Code only picks the 1h cache TTL itself on direct subscription auth.
    promptCacheTtl = "1h";
    env.ANTHROPIC_BASE_URL = cfg.cliproxy.url;
  };
  codexSettings = {
    model = models.defaults.codex;
    model_provider = "cliproxy";
    model_providers.cliproxy = {
      name = "CLIProxyAPI";
      base_url = "${cfg.cliproxy.url}/v1";
      wire_api = "responses";
      requires_openai_auth = false;
      env_key = "CLIPROXY_API_KEY";
    };
  };
  piDefaults = {
    defaultProvider = "cliproxy";
    defaultModel = models.defaults.pi;
    defaultThinkingLevel = "high";
  };
  piPackages = profile: { packages = [ "${pluginsStateDir}/profiles/${profile}" ]; };
  piModels.providers.cliproxy = import ../dev/llms/cliproxy-pi-provider.nix {
    inherit (cfg.cliproxy) url;
  };
  t3Settings = {
    linkedEnvironmentsEnabled = true;
    enableProviderUpdateChecks = false;
    providerInstances = {
      claudeAgent = {
        driver = "claudeAgent";
        config.homePath = dirs.claude;
      };
      codex = {
        driver = "codex";
        config.homePath = dirs.codex;
      };
      pi = {
        driver = "pi";
        environment = [
          {
            name = "PI_CODING_AGENT_DIR";
            value = dirs.piT3;
            sensitive = false;
          }
        ];
      };
      antigravity = {
        driver = "antigravity";
        config.binaryPath = lib.getExe antigravity;
      };
    };
  };
  t3Defaults.providerInstances =
    lib.mapAttrs (_: _: { enabled = true; }) t3Settings.providerInstances
    // {
      pi = {
        displayName = "Pi";
        enabled = true;
      };
    };
  t3Environment = ''
    .providerInstances.pi.environment = [
      ($user.providerInstances.pi.environment // [])[]
      | select(.name | IN($managed[0].providerInstances.pi.environment[].name) | not)
    ] + $managed[0].providerInstances.pi.environment
  '';
  pluginsSyncConfig = pkgs.writeText "ol-plugins-sync.edn" ''
    {:state-dir ${builtins.toJSON pluginsStateDir}
     :t3-settings ${builtins.toJSON "${t3Home}/userdata/settings.json"}
     :discover-t3 true
     :include-antigravity-global true
     :codex-bin ${builtins.toJSON (lib.getExe agents.codex)}
     :retain-revs 2
     :peers []
     :packs {"harness-plugins" {:url "https://git.outskirtslabs.com/casey/skills.git" :branch "main"}}
     :defaults {:plugins ["my@harness-plugins" "pstack@harness-plugins" "tstack@harness-plugins"]}
     :profiles {"pi-cli" {:harness :pi :dir ${builtins.toJSON dirs.pi}}}}
  '';

  json5 = pkgs.python3Packages.toPythonApplication pkgs.python3Packages.json5;
  jq = lib.getExe pkgs.jq;
  remarshal = lib.getExe' pkgs.remarshal "remarshal";
  # Paths are relative to the home directory: creating any component of the
  # /home mount fails on virtiofs, even when it exists.
  rel = lib.removePrefix "${home}/";
  mergeJson =
    absolute:
    {
      managed,
      defaults ? { },
      filter ? ".",
    }:
    let
      path = rel absolute;
    in
    ''
      install -d -m 0700 "$(dirname ${path})"
      [ -s ${path} ] || echo '{}' > ${path}
      ${lib.getExe json5} --as-json < ${path} \
        | ${jq} \
          --slurpfile defaults ${json.generate "defaults.json" defaults} \
          --slurpfile managed ${json.generate "managed.json" managed} \
          ${lib.escapeShellArg "($defaults[0] * .) as $user | $user * $managed[0] | ${filter}"} > ${path}.new
      mv ${path}.new ${path}
    '';
  mergeToml =
    absolute: settings:
    let
      path = rel absolute;
    in
    ''
      install -d -m 0700 "$(dirname ${path})"
      [ -s ${path} ] || : > ${path}
      ${remarshal} -if toml -of json < ${path} \
        | ${jq} --slurpfile static ${json.generate "config.json" settings} '. * $static[0]' \
        | ${remarshal} -if json -of toml > ${path}.new
      mv ${path}.new ${path}
    '';
  writeConfig = absolute: file: ''
    install -D -m 0644 ${file} ${rel absolute}
  '';
  configure = pkgs.writeShellScript "moot-wright-config" ''
    set -euo pipefail
    umask 077
    cd ${home}
    ${mergeJson "${dirs.claude}/settings.json" {
      managed = claudeSettings;
      filter = "del(.env.ANTHROPIC_AUTH_TOKEN, .env.ANTHROPIC_API_KEY)";
    }}
    ${mergeToml "${dirs.codex}/config.toml" codexSettings}
    ${mergeJson "${t3Home}/userdata/settings.json" {
      managed = t3Settings;
      defaults = t3Defaults;
      filter = t3Environment;
    }}
    ${lib.concatMapStrings
      (
        { dir, profile }:
        mergeJson "${dir}/settings.json" {
          managed = piPackages profile;
          defaults = piDefaults;
        }
        + mergeJson "${dir}/models.json" {
          managed = piModels;
          filter = ".providers.cliproxy = $managed[0].providers.cliproxy";
        }
      )
      [
        {
          dir = dirs.pi;
          profile = "pi-cli";
        }
        {
          dir = dirs.piT3;
          profile = "pi";
        }
      ]
    }
    ${writeConfig "${configHome}/ol-plugins-sync/config.edn" pluginsSyncConfig}
  '';

  configUnit = "moot-wright-config.service";
  builder = cfg.nix.remoteBuilder;
in
{
  options.modules.microvm-guest.mootWright = {
    enable = lib.mkEnableOption "a confined Moot wright: one agent user whose traffic leaves only through its own tailnet node";

    name = lib.mkOption {
      type = lib.types.strMatching "[a-z][a-z0-9-]{0,15}";
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "The wright's name: its Unix user and identity.";
    };

    t3Port = lib.mkOption {
      type = lib.types.port;
      default = 3780;
      description = "Port T3 listens on at the guest address, the target of the node's tailnet HTTPS.";
    };

    cliproxy.url = lib.mkOption {
      type = lib.types.str;
      apply = lib.removeSuffix "/";
      default = "https://moot-llm.${tailnet}";
      defaultText = lib.literalExpression ''"https://moot-llm.''${tailnet}"'';
      description = "CLIProxyAPI root URL, without /v1. It knows the wright by its tailnet identity.";
    };

    nix = {
      maxJobs = lib.mkOption {
        type = lib.types.ints.unsigned;
        default = if cfg.nix.remoteBuilder == null then lib.max 1 (config.microvm.vcpu / 2) else 0;
        defaultText = lib.literalExpression "if remoteBuilder == null then max 1 (microvm.vcpu / 2) else 0";
        description = "Local build jobs; 0 builds only on the remote builder.";
      };
      remoteBuilder = lib.mkOption {
        default = null;
        description = ''
          Remote builder reached over ssh-ng through the node's proxy. The
          builder's node knows the guest by its tailnet identity, so the guest
          holds no SSH key. Without one, the guest builds locally.
        '';
        type = lib.types.nullOr (
          lib.types.submodule {
            options = {
              hostName = lib.mkOption {
                type = lib.types.str;
                description = "Builder hostname; tailnet names resolve through the node's proxy.";
              };
              publicHostKey = lib.mkOption {
                type = lib.types.str;
                description = "The SSH host key of the builder's node, as an OpenSSH public key line.";
              };
              systems = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ pkgs.stdenv.hostPlatform.system ];
                defaultText = lib.literalExpression "[ pkgs.stdenv.hostPlatform.system ]";
                description = "Systems the builder builds for.";
              };
              maxJobs = lib.mkOption {
                type = lib.types.ints.positive;
                default = 4;
                description = "Concurrent builds on the builder.";
              };
              speedFactor = lib.mkOption {
                type = lib.types.ints.positive;
                default = 1;
                description = "Relative builder speed.";
              };
              supportedFeatures = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [
                  "benchmark"
                  "big-parallel"
                ];
                description = "System features the builder supports.";
              };
            };
          }
        );
      };
    };

    identity = {
      gitName = lib.mkOption {
        type = lib.types.str;
        default = name;
        defaultText = lib.literalExpression "config.modules.microvm-guest.mootWright.name";
        description = "Git author name.";
      };
      gitEmail = lib.mkOption {
        type = lib.types.str;
        default = "${name}@moot.land";
        defaultText = lib.literalExpression ''"''${name}@moot.land"'';
        description = "Git author email.";
      };
      matrixId = lib.mkOption {
        type = lib.types.str;
        default = "@${name}:moot.land";
        defaultText = lib.literalExpression ''"@''${name}:moot.land"'';
        description = "Matrix ID, exported as MOOT_MATRIX_ID.";
      };
    };

    credentials = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "[A-Z][A-Z0-9_]*");
      default = [ ];
      description = ''
        Credentials the egress injects for this wright. Each is exported as a
        variable of the same name holding the placeholder
        TS_EGRESS_PLACEHOLDER_<NAME>; the egress swaps in the secret.
      '';
    };

    extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Additional system packages for the wright.";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.motd.units = [ config.systemd.services."t3-${name}".name ];
    modules.microvm-guest = {
      mootVm = {
        enable = true;
        network.ingressPort = cfg.t3Port;
      };
      mounts = [
        "var/lib"
        "home"
      ];
    };

    programs.git = {
      enable = true;
      config = {
        user.name = cfg.identity.gitName;
        user.email = cfg.identity.gitEmail;
        init.defaultBranch = "main";
      };
    };

    environment.systemPackages =
      providers
      ++ [
        t3
        pkgs.harness-plugins-sync
      ]
      ++ cfg.extraPackages;
    environment.extraInit = ''
      if [ "$(id -u)" = ${toString uid} ]; then
      ${exports wrightEnv}
      fi
    '';

    nix = {
      settings.max-jobs = cfg.nix.maxJobs;
      distributedBuilds = builder != null;
      buildMachines = lib.optional (builder != null) {
        inherit (builder)
          hostName
          systems
          maxJobs
          speedFactor
          supportedFeatures
          ;
        protocol = "ssh-ng";
      };
    };
    programs.ssh = lib.mkIf (builder != null) {
      knownHosts.${builder.hostName}.publicKey = builder.publicHostKey;
      extraConfig = ''
        Host ${builder.hostName}
          ProxyCommand ${lib.getExe' pkgs.netcat-openbsd "nc"} -X connect -x ${proxyAddress} %h %p
      '';
    };

    users = {
      groups.${name}.gid = uid;
      users.${name} = {
        isNormalUser = true;
        inherit uid home;
        group = name;
        createHome = true;
        # T3 starts agents in scopes under this user's systemd manager.
        linger = true;
      };
    };
    systemd.tmpfiles.rules = [ "d ${home} 0700 ${name} ${name} -" ];

    systemd.services = {
      moot-wright-config = {
        description = "Merge managed agent settings for ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "systemd-tmpfiles-setup.service" ];
        unitConfig.RequiresMountsFor = [ home ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = name;
          Group = name;
          ExecStart = configure;
        };
      };

      "user@${toString uid}" = {
        overrideStrategy = "asDropin";
        wants = [ configUnit ];
        after = [ configUnit ];
      };

      "t3-${name}" = {
        description = "T3 Code server for ${name}";
        wantedBy = [ "multi-user.target" ];
        # T3 probes for agent scopes once at startup, so the user manager
        # must already be running.
        wants = [
          "network-online.target"
          "user@${toString uid}.service"
        ];
        after = [
          "network-online.target"
          "user@${toString uid}.service"
        ];
        path = providers ++ [
          t3
          pkgs.git
          pkgs.bash
        ];
        environment = wrightEnv // {
          HOME = home;
        };
        serviceConfig = {
          User = name;
          Group = name;
          WorkingDirectory = home;
          ExecStart = "${lib.getExe' t3 "t3"} serve --host ${vm.network.guestAddress} --port ${toString cfg.t3Port} ${home}";
          Restart = "on-failure";
          RestartSec = 5;
        };
      };
    };

    systemd.user.services.harness-plugins-sync = {
      description = "Sync agent plugin packs and update harness bindings";
      unitConfig.ConditionUser = name;
      environment = wrightEnv;
      path = [ pkgs.git ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${lib.getExe pkgs.harness-plugins-sync} sync";
      };
    };
    systemd.user.timers.harness-plugins-sync = {
      description = "Periodically sync agent plugin packs";
      wantedBy = [ "timers.target" ];
      unitConfig.ConditionUser = name;
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "15min";
        Persistent = true;
      };
    };
  };
}
