{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.matrix-synapse.bridges.discord;
  rootCfg = config.modules.services.matrix-synapse;
  dataDir = "${rootCfg.dataDir}/mautrix-discord";
  registrationFile = config.sops.secrets."mautrix-discord/registration.yaml".path;
  doublepuppetFile = config.sops.secrets."doublepuppet.yaml".path;
  settingsFormat = pkgs.formats.yaml { };
  settingsFileUnsubstituted = settingsFormat.generate "mautrix-discord-config-unsubstituted.yaml" {
    homeserver = {
      address = "https://${rootCfg.domain}";
      domain = rootCfg.serverName;
      async_media = true;
    };
    appservice = {
      address = "http://127.0.0.1:${toString cfg.ports.http}";
      hostname = "127.0.0.1";
      port = cfg.ports.http;
      database = {
        type = "postgres";
        uri = "postgresql:///mautrix-discord?sslmode=disable&host=/run/postgresql-matrix-synapse";
      };
      ephemeral_events = true;
      async_transactions = true;
      as_token = "$AS_TOKEN";
      hs_token = "$HS_TOKEN";
    };
    logging = {
      file_name_format = null;
    };
    metrics = {
      enabled = false;
      listen = "127.0.0.1:29321";
    };
    bridge = {
      login_shared_secret_map = {
        ${rootCfg.serverName} = "as_token:$DOUBLEPUPPET_AS_TOKEN";
      };
      channel_name_template = "{{if or (eq .Type 3) (eq .Type 4)}}{{.Name}} ({{.GuildName}} — {{.ParentName}}){{else}}#{{.Name}} ({{.GuildName}} — {{.ParentName}}){{end}}";
      private_chat_portal_meta = "always";
      startup_private_channel_create_limit = 25;
      delivery_receipts = true;
      sync_direct_chat_list = true;
      delete_portal_on_channel_delete = true;
      prefix_webhook_messages = true;
      cache_media = "always";
      animated_sticker.target = "disable";
      backfill = {
        forward_limits = {
          initial.dm = 50;
          initial.channel = 50;
          initial.thread = 50;

          missed.dm = -1;
          missed.channel = -1;
          missed.thread = -1;
        };
      };
      encryption = {
        allow = true;
        default = false;
        appservice = false;
        require = false;
        plaintext_mentions = true;
        allow_key_sharing = true;
      };
      permissions = {
        "*" = "relay";
        "@ramblurr:${rootCfg.serverName}" = "admin";
      };
    };
  };

  settingsFile = "${dataDir}/config.yaml";
in
{
  options.modules.services.matrix-synapse.bridges.discord = {
    enable = lib.mkEnableOption "Mautrix-Whatsapp, a Matrix-Whatsapp hybrid puppeting/relaybot bridge";
    settings = lib.mkOption rec {
      apply = lib.recursiveUpdate lib.default;
      inherit (settingsFormat) type;
    };

    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
      };
    };
    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        File containing environment variables to be passed to the mautrix-telegram service,
        in which secret tokens can be specified securely by defining values for
        <literal>MAUTRIX_XXX</literal>,
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      inherit (cfg.user) uid;
      useDefaultShell = true;
      isSystemUser = true;
      group = cfg.group.name;
      extraGroups = [ rootCfg.bridgesGroup.name ];
    };

    users.groups.${cfg.group.name} = {
      gid = lib.mkForce cfg.group.gid;
    };

    sops.secrets."mautrix-discord/registration.yaml" = {
      sopsFile = ../../configs/home-ops/matrix-synapse.sops.yaml;
      owner = cfg.user.name;
      group = rootCfg.bridgesGroup.name;
      mode = "0440";
    };

    systemd.tmpfiles.rules = [
      "d '${dataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "Z '${dataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];

    services.matrix-synapse.settings.app_service_config_files = [
      config.sops.secrets."mautrix-discord/registration.yaml".path
    ];

    systemd.services.mautrix-discord-genregistration = {
      description = "Mautrix-Discord Registration";
      script = ''
        # Not all secrets can be passed as environment variable (yet)
        # https://github.com/tulir/mautrix-telegram/issues/584
        [ -f ${settingsFile} ] && rm -f ${settingsFile}
        export AS_TOKEN=$(${pkgs.yq}/bin/yq -r '.as_token' ${registrationFile})
        export HS_TOKEN=$(${pkgs.yq}/bin/yq -r '.hs_token' ${registrationFile})
        export DOUBLEPUPPET_AS_TOKEN=$(${pkgs.yq}/bin/yq -r '.as_token' ${doublepuppetFile})
        umask 0177
        ${pkgs.envsubst}/bin/envsubst \
          -o ${settingsFile} \
          -i ${settingsFileUnsubstituted}
      '';

      unitConfig = {
        RequiresMountsFor = [ dataDir ];
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ReadWritePaths = [ rootCfg.dataDir ];
        NoNewPrivileges = true;
        MemoryDenyWriteExecute = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictRealtime = true;
        LockPersonality = true;
        ProtectKernelLogs = true;
        ProtectKernelTunables = true;
        ProtectHostname = true;
        ProtectKernelModules = true;
        ProtectClock = true;
        SystemCallArchitectures = "native";
        SystemCallErrorNumber = "EPERM";
        SystemCallFilter = "@system-service";
        WorkingDirectory = dataDir;
        StateDirectory = baseNameOf dataDir;
        UMask = 117;
        User = cfg.user.name;
        Group = cfg.group.name;
        EnvironmentFile = cfg.environmentFile;
      };
      restartTriggers = [
        settingsFileUnsubstituted
        cfg.environmentFile
      ];
    };
    systemd.services.mautrix-discord = {
      enable = true;
      description = "Mautrix-Discord";
      path = with pkgs; [
        ffmpeg
        lottieconverter
      ];
      wantedBy = [ "multi-user.target" ];
      wants = [
        "mautrix-discord-genregistration.service"
        "postgresql.service"
      ];
      after = [
        "mautrix-discord-genregistration.service"
        "postgresql.service"
      ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "30";
        ReadWritePaths = [ rootCfg.dataDir ];
        NoNewPrivileges = true;
        MemoryDenyWriteExecute = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictRealtime = true;
        LockPersonality = true;
        ProtectKernelLogs = true;
        ProtectKernelTunables = true;
        ProtectHostname = true;
        ProtectKernelModules = true;
        ProtectClock = true;
        SystemCallArchitectures = "native";
        SystemCallErrorNumber = "EPERM";
        SystemCallFilter = "@system-service";
        WorkingDirectory = dataDir;
        StateDirectory = baseNameOf dataDir;
        UMask = 117;
        User = cfg.user.name;
        Group = cfg.group.name;
        EnvironmentFile = cfg.environmentFile;
        ExecStart = ''
          ${pkgs.mautrix-discord}/bin/mautrix-discord \
            --config='${settingsFile}'
        '';
      };
      restartTriggers = [ cfg.environmentFile ];
    };
  };
}
