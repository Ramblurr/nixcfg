{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hosts.james.webhooks;
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

        user = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            Optional per-hook systemd user.
            If unset, hosts.james.webhooks.user is used.
          '';
        };

        group = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            Optional per-hook systemd group.
            If unset, hosts.james.webhooks.group is used.
          '';
        };

        socketPath = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            Optional per-hook Unix socket path.
            If unset, a deterministic path is generated in hosts.james.webhooks.socketDirectory.
          '';
        };

        extraPath = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [ ];
          description = "Extra packages added to this hook service PATH.";
        };
      };
    }
  );

  sanitizeHookId = hookId: lib.strings.sanitizeDerivationName hookId;

  mkHookEntry =
    name: hook:
    let
      resolvedSecretsFile = if hook.secretsFile == null then cfg.secretsFile else hook.secretsFile;
      hookId = hook.id;
      safeId = sanitizeHookId hookId;
      serviceBaseName = "webhook-${cfg.serviceName}-${safeId}";
    in
    {
      inherit
        name
        hook
        hookId
        safeId
        ;
      missingSecretsFile = resolvedSecretsFile == null;
      secretsFile = if resolvedSecretsFile == null then "__missing__" else resolvedSecretsFile;
      serviceName = serviceBaseName;
      serviceUnit = "${serviceBaseName}.service";
      socketPath =
        if hook.socketPath != null then
          hook.socketPath
        else
          "${cfg.socketDirectory}/github-${cfg.serviceName}-${safeId}.sock";
      user = if hook.user == null then cfg.user else hook.user;
      group = if hook.group == null then cfg.group else hook.group;
      extraPath = cfg.extraPath ++ hook.extraPath ++ [ pkgs.coreutils ];
    };

  hookEntriesByName = lib.mapAttrs mkHookEntry cfg.hooks;
  hookEntries = builtins.attrValues hookEntriesByName;

  countBy = xs: lib.foldl' (acc: x: acc // { ${x} = (acc.${x} or 0) + 1; }) { } xs;
  hookIds = map (hookEntry: hookEntry.hookId) hookEntries;
  duplicateHookIds = lib.attrNames (lib.filterAttrs (_: count: count > 1) (countBy hookIds));
  missingHookIds = map (hookEntry: hookEntry.hookId) (
    lib.filter (hookEntry: hookEntry.missingSecretsFile) hookEntries
  );

  secretToken = secretsFile: builtins.substring 0 12 (builtins.hashString "sha256" secretsFile);
  secretEnvVar = secretsFile: "WEBHOOK_SECRET_" + secretToken secretsFile;
  secretCredential = secretsFile: "WEBHOOK_SECRET_FILE_" + secretToken secretsFile;
  secretPlaceholder = secretsFile: "WEBHOOK_SECRET_PLACEHOLDER_" + secretToken secretsFile;

  mkHookConfig =
    hookEntry:
    let
      hookAttrs = builtins.removeAttrs hookEntry.hook [
        "secretsFile"
        "user"
        "group"
        "socketPath"
        "extraPath"
      ];
      signatureRule = {
        match = {
          type = cfg.signatureType;
          secret = secretPlaceholder hookEntry.secretsFile;
          parameter = {
            source = "header";
            name = cfg.signatureHeader;
          };
        };
      };
      renderedHook = hookAttrs // {
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
      };
      hookConfig = builtins.toJSON [ renderedHook ];
    in
    pkgs.writeText "hook-config-${hookEntry.safeId}.json" (
      builtins.replaceStrings
        [ (secretPlaceholder hookEntry.secretsFile) ]
        [ ''{{ getenv "${secretEnvVar hookEntry.secretsFile}" | js }}'' ]
        hookConfig
    );

  hookSocketPaths = lib.listToAttrs (
    map (hookEntry: lib.nameValuePair hookEntry.hookId hookEntry.socketPath) hookEntries
  );
  hookServiceNames = lib.listToAttrs (
    map (hookEntry: lib.nameValuePair hookEntry.hookId hookEntry.serviceUnit) hookEntries
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

    socketDirectory = lib.mkOption {
      type = lib.types.str;
      default = "/var/run/nginx";
      description = "Directory used for generated per-hook Unix sockets.";
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

    hookSocketPaths = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      description = "Computed map of hook id -> Unix socket path.";
    };

    hookServiceNames = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      description = "Computed map of hook id -> systemd service unit name.";
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
      {
        assertion = duplicateHookIds == [ ];
        message = ''
          Hook ids must be unique across hosts.james.webhooks.hooks.
          Duplicate ids: ${builtins.concatStringsSep ", " duplicateHookIds}
        '';
      }
    ];

    hosts.james.webhooks.hookSocketPaths = hookSocketPaths;
    hosts.james.webhooks.hookServiceNames = hookServiceNames;

    systemd.services = lib.mapAttrs' (
      _name: hookEntry:
      lib.nameValuePair hookEntry.serviceName {
        description = "Webhook listener (${cfg.serviceName}/${hookEntry.hookId})";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        script = ''
          export ${secretEnvVar hookEntry.secretsFile}="$(cat "$CREDENTIALS_DIRECTORY/${secretCredential hookEntry.secretsFile}")"
          exec ${pkgs.webhook}/bin/webhook \
            -urlprefix ${cfg.urlPrefix} \
            -template \
            -hooks ${mkHookConfig hookEntry} \
            -verbose \
            -socket ${hookEntry.socketPath}
        '';
        restartIfChanged = true;
        serviceConfig = {
          LoadCredential = [ "${secretCredential hookEntry.secretsFile}:${hookEntry.secretsFile}" ];
          Restart = "always";
          User = hookEntry.user;
          Group = hookEntry.group;
          UMask = "0007";
        };
        path = hookEntry.extraPath;
      }
    ) hookEntriesByName;
  };
}
