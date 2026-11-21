{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  syncCfg = cfg.pluginsSync;
  primaryHome = config.modules.users.primaryUser.homeDirectory;

  ednScalar =
    v:
    if v == null then
      "nil"
    else if builtins.isBool v then
      (if v then "true" else "false")
    else if builtins.isInt v || builtins.isFloat v then
      toString v
    else if builtins.isString v then
      builtins.toJSON v
    else if builtins.isList v then
      "[${lib.concatStringsSep " " (map ednScalar v)}]"
    else
      throw "plugins-sync: unsupported EDN scalar value";

  ednKeyword = name: ":${name}";

  ednPack =
    name: pack:
    "${builtins.toJSON name} {:url ${builtins.toJSON pack.url} :branch ${builtins.toJSON pack.branch}}";

  ednProfile =
    name: prof:
    let
      entries =
        lib.optional (prof.harness != null) ":harness ${ednKeyword prof.harness}"
        ++ lib.optional (prof.dir != null) ":dir ${builtins.toJSON prof.dir}"
        ++ lib.optional (prof.linkDir != null) ":link-dir ${builtins.toJSON prof.linkDir}"
        ++ lib.optional (prof.enable != null) ":enabled ${ednScalar prof.enable}"
        ++ lib.optional (prof.plugins != null) ":plugins ${ednScalar prof.plugins}"
        ++ lib.optional (prof.extraPlugins != [ ]) ":extra-plugins ${ednScalar prof.extraPlugins}"
        ++ lib.optional (prof.excludePlugins != [ ]) ":exclude-plugins ${ednScalar prof.excludePlugins}";
    in
    "${builtins.toJSON name} {${lib.concatStringsSep " " entries}}";

  renderConfigEdn =
    {
      stateDir,
      t3Settings,
      discoverT3,
      includeAntigravityGlobal,
      codexBin,
      retainRevs,
      peers,
      packs,
      defaultPlugins,
      profiles,
    }:
    let
      packLines = lib.mapAttrsToList ednPack packs;
      profileLines = lib.mapAttrsToList ednProfile profiles;
    in
    ''
      {:state-dir ${builtins.toJSON stateDir}
       :t3-settings ${ednScalar t3Settings}
       :discover-t3 ${ednScalar discoverT3}
       :include-antigravity-global ${ednScalar includeAntigravityGlobal}
       :codex-bin ${builtins.toJSON codexBin}
       :retain-revs ${toString retainRevs}
       :peers ${ednScalar peers}
       :packs {${lib.concatStringsSep "\n         " packLines}}
       :defaults {:plugins ${ednScalar defaultPlugins}}
       :profiles {${lib.concatStringsSep "\n            " profileLines}}}
    '';

  packSubmodule = lib.types.submodule {
    options = {
      url = lib.mkOption {
        type = lib.types.str;
        description = "Git URL for this plugin pack.";
      };
      branch = lib.mkOption {
        type = lib.types.str;
        default = "main";
        description = "Git branch tracked for this plugin pack.";
      };
    };
  };

  profileSubmodule = lib.types.submodule {
    options = {
      harness = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "claude"
            "codex"
            "pi"
            "antigravity"
          ]
        );
        default = null;
        description = "Target harness. Omit when overriding a T3-discovered profile by instance ID.";
      };
      dir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Profile home directory. Omit when overriding a T3-discovered profile.";
      };
      linkDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional custom projection directory for Pi profiles.";
      };
      enable = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Set false to disable binding for a T3-discovered profile.";
      };
      plugins = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "Explicit plugin@pack list replacing defaults for this profile.";
      };
      extraPlugins = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional plugin@pack selectors appended to defaults.";
      };
      excludePlugins = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Plugin selectors removed from this profile.";
      };
    };
  };
in
{
  options.modules.dev.llms.pluginsSync = {
    enable = lib.mkEnableOption "harness-plugins-sync configuration and periodic sync timer";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.harness-plugins-sync;
      description = "Package providing the `harness-plugins-sync` CLI.";
    };
    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "${primaryHome}/.local/share/ol-plugins-sync";
      defaultText = lib.literalExpression ''"''${config.modules.users.primaryUser.homeDirectory}/.local/share/ol-plugins-sync"'';
      description = "State directory for pack mirrors, active snapshots, and per-profile bindings.";
    };
    t3Settings = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "${primaryHome}/.config/t3/userdata/settings.json";
      defaultText = lib.literalExpression ''"''${config.modules.users.primaryUser.homeDirectory}/.config/t3/userdata/settings.json"'';
      description = "Path to T3 Code's settings.json for automatic provider instance discovery.";
    };
    discoverT3 = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Discover harness profiles from T3 Code providerInstances.";
    };
    includeAntigravityGlobal = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Also bind standalone Antigravity CLI plugins under ~/.gemini.";
    };
    retainRevs = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 2;
      description = "Number of inactive unpacked Git revisions to retain per pack.";
    };
    peers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "${config.modules.users.primaryUser.username}@quine"
        "${config.modules.users.primaryUser.username}@witt"
        "${config.modules.users.primaryUser.username}@thingstead"
      ];
      description = "SSH peer hostnames targeted by `harness-plugins-sync sync --all`.";
    };
    packs = lib.mkOption {
      type = lib.types.attrsOf packSubmodule;
      default = {
        harness-plugins = {
          url = "https://git.outskirtslabs.com/casey/skills.git";
          branch = "main";
        };
      };
      description = "Git plugin packs managed by `harness-plugins-sync`.";
    };
    defaultPlugins = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "my@harness-plugins"
        "pstack@harness-plugins"
        "tstack@harness-plugins"
      ];
      description = "Default plugin@pack selectors bound to every profile unless overridden.";
    };
    profiles = lib.mkOption {
      type = lib.types.attrsOf profileSubmodule;
      default = { };
      description = "Declared harness profiles and overrides merged with T3 discovery.";
    };
    timer = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Run `harness-plugins-sync sync` periodically via a systemd user timer.";
      };
      interval = lib.mkOption {
        type = lib.types.str;
        default = "15min";
        description = "Systemd timer interval between automatic sync runs.";
      };
    };
  };

  config = lib.mkIf (cfg.enable && syncCfg.enable) {
    myhm = {
      home.packages = [ syncCfg.package ];

      xdg.configFile."ol-plugins-sync/config.edn".text = renderConfigEdn {
        inherit (syncCfg)
          stateDir
          t3Settings
          discoverT3
          includeAntigravityGlobal
          retainRevs
          peers
          packs
          defaultPlugins
          profiles
          ;
        codexBin = if cfg.codex.enable then lib.getExe cfg.codex.package else "codex";
      };

      systemd.user.services.harness-plugins-sync = lib.mkIf syncCfg.timer.enable {
        Unit = {
          Description = "Sync agent plugin packs and update harness bindings";
          After = [ "network-online.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${lib.getExe syncCfg.package} sync";
        };
      };

      systemd.user.timers.harness-plugins-sync = lib.mkIf syncCfg.timer.enable {
        Unit = {
          Description = "Periodically sync agent plugin packs";
        };
        Timer = {
          OnBootSec = "1min";
          OnUnitActiveSec = syncCfg.timer.interval;
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
