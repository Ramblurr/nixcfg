{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hosts.james.webhooks;
  serviceName = "webhook-${cfg.serviceName}";
  hookType = lib.types.submodule (
    { name, ... }:
    {
      freeformType = lib.types.attrs;
      options = {
        id = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Hook identifier exposed to webhook(8). Defaults to the attribute key.";
        };

        secretsFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            Optional path to a file containing this hook's signature secret.
            If unset, hosts.james.webhooks.secretsFile is used.
          '';
        };
      };
    }
  );
  hooks = builtins.attrValues cfg.hooks;
  hookSecrets = map (
    hook:
    let
      resolvedSecretsFile = if hook.secretsFile == null then cfg.secretsFile else hook.secretsFile;
    in
    {
      inherit hook;
      missing = resolvedSecretsFile == null;
      secretsFile = if resolvedSecretsFile == null then "__missing__" else resolvedSecretsFile;
    }
  ) hooks;
  missingHookIds = map (x: x.hook.id) (lib.filter (x: x.missing) hookSecrets);
  secretToken = secretsFile: builtins.substring 0 12 (builtins.hashString "sha256" secretsFile);
  secretEnvVar = secretsFile: "WEBHOOK_SECRET_" + secretToken secretsFile;
  secretCredential = secretsFile: "WEBHOOK_SECRET_FILE_" + secretToken secretsFile;
  secretPlaceholder = secretsFile: "WEBHOOK_SECRET_PLACEHOLDER_" + secretToken secretsFile;
  secretFiles = lib.unique (map (x: x.secretsFile) (lib.filter (x: !x.missing) hookSecrets));
  renderedHooks = map (
    hookSecret:
    let
      inherit (hookSecret) secretsFile;
      hookAttrs = builtins.removeAttrs hookSecret.hook [ "secretsFile" ];
      signatureRule = {
        match = {
          type = cfg.signatureType;
          secret = secretPlaceholder secretsFile;
          parameter = {
            source = "header";
            name = cfg.signatureHeader;
          };
        };
      };
    in
    hookAttrs
    // {
      trigger-rule =
        if hookAttrs ? trigger-rule then
          {
            and = [
              signatureRule
              hookAttrs.trigger-rule
            ];
          }
        else
          signatureRule;
    }
  ) hookSecrets;
  secretExports = lib.concatMapStringsSep "\n" (secretsFile: ''
    export ${secretEnvVar secretsFile}="$(cat "$CREDENTIALS_DIRECTORY/${secretCredential secretsFile}")"
  '') secretFiles;
  hookConfig = pkgs.writeText "hook-config.json" (
    builtins.replaceStrings (map secretPlaceholder secretFiles) (map (
      secretsFile: ''{{ getenv "${secretEnvVar secretsFile}" | js }}''
    ) secretFiles) (builtins.toJSON renderedHooks)
  );
in
{
  options.hosts.james.webhooks = {
    enable = lib.mkEnableOption "GitHub webhook listener";

    serviceName = lib.mkOption {
      type = lib.types.str;
      default = "site";
      description = "Suffix for the systemd unit name.";
    };

    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "/var/run/nginx/github-hook.sock";
      description = "Unix socket path for the webhook server.";
    };

    urlPrefix = lib.mkOption {
      type = lib.types.str;
      default = "_deploy";
      description = "URL prefix passed to the webhook daemon.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "nginx";
      description = "User that runs the webhook service.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "nginx";
      description = "Group that runs the webhook service.";
    };

    secretsFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Default path to a file containing the webhook signature secret.";
    };

    signatureType = lib.mkOption {
      type = lib.types.str;
      default = "payload-hmac-sha1";
      description = "Webhook signature validation mode.";
    };

    signatureHeader = lib.mkOption {
      type = lib.types.str;
      default = "X-Hub-Signature";
      description = "Header containing the webhook signature.";
    };

    hooks = lib.mkOption {
      type = lib.types.attrsOf hookType;
      default = { };
      description = ''
        Hook definitions keyed by hook id, consumed by webhook(8) without the signature rule.
        The module prepends the signature rule automatically, and the id defaults to the key.
        Each hook can override secretsFile.
      '';
    };

    extraPath = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Extra packages added to the webhook service PATH.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.hooks != { };
        message = "hosts.james.webhooks.enable is true but no hooks were configured.";
      }
      {
        assertion = missingHookIds == [ ];
        message = ''
          Each hook must set secretsFile or hosts.james.webhooks.secretsFile.
          Missing secretsFile for hooks: ${builtins.concatStringsSep ", " missingHookIds}
        '';
      }
    ];

    systemd.services.${serviceName} = {
      description = "Webhook listener (${cfg.serviceName})";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
        ${secretExports}
        exec ${pkgs.webhook}/bin/webhook \
          -urlprefix ${cfg.urlPrefix} \
          -template \
          -hooks ${hookConfig} \
          -verbose \
          -socket ${cfg.socketPath}
      '';
      restartIfChanged = true;
      serviceConfig = {
        LoadCredential = map (secretsFile: "${secretCredential secretsFile}:${secretsFile}") secretFiles;
        Restart = "always";
        User = cfg.user;
        Group = cfg.group;
      };
      path = cfg.extraPath ++ [ pkgs.coreutils ];
    };
  };
}
