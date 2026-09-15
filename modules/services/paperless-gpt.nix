{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.paperless-gpt;
  socket = "/run/paperless-gpt/http.sock";
  socketGroup = "paperless-gpt-proxy";
in
{
  options.modules.services.paperless-gpt = {
    enable = lib.mkEnableOption "Paperless GPT with Mistral OCR";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "Hostname for the authenticated, private-only Caddy route.";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties."rpool/encrypted/safe/svc/paperless-gpt".mountpoint =
      "/var/lib/private/paperless-gpt";

    assertions = [
      {
        assertion = config.services.paperless.enable;
        message = "Paperless GPT requires local Paperless-ngx.";
      }
      {
        assertion = config.modules.services.onepassword-systemd-credentials.enable;
        message = "Paperless GPT requires the 1Password credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.paperless-gpt = {
      paperless-api-token = "op://home-ops-prod/paperless paperless-gpt/api-token";
      mistral-api-key = "op://home-ops-prod/paperless/mistral-api-key";
    };
    users.groups.${socketGroup} = { };
    users.users.${config.services.caddy.user}.extraGroups = [ socketGroup ];

    systemd.services.paperless-gpt = {
      description = "Paperless GPT metadata and Mistral OCR sidecar";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "paperless-web.service"
      ];
      requires = [ "paperless-web.service" ];
      unitConfig.RequiresMountsFor = [ "/var/lib/private/paperless-gpt" ];
      environment = {
        PAPERLESS_BASE_URL = "http://127.0.0.1:${toString config.services.paperless.port}";
        PAPERLESS_PUBLIC_URL = config.services.paperless.settings.PAPERLESS_URL;
        LISTEN_SOCKET = socket;
        LLM_PROVIDER = "mistral";
        LLM_MODEL = "mistral-small-latest";
        TOKEN_LIMIT = "16384";
        OCR_PROVIDER = "mistral_ocr";
        MISTRAL_MODEL = "mistral-ocr-latest";
        # Pinned upstream deletes the Files API upload after OCR (best-effort).
        OCR_PROCESS_MODE = "whole_pdf";
        LOG_LEVEL = "info";
        GIN_MODE = "release";
        MANUAL_TAG = "paperless-gpt";
        AUTO_TAG = "paperless-gpt-auto";
        AUTO_OCR_TAG = "paperless-gpt-ocr-auto";
        PDF_UPLOAD = "false";
        PDF_REPLACE = "false";
        CREATE_LOCAL_PDF = "false";
        CREATE_LOCAL_HOCR = "false";
        CREATE_NEW_TAGS = "false";
        PRESERVE_EXISTING_METADATA = "true";
      };
      preStart = ''
        ln -sfn ${pkgs.paperless-gpt}/share/paperless-gpt/default_prompts default_prompts
        rm -f -- ${socket}
      '';
      # Read credentials only in the service process, never into a store file or log.
      script = ''
        set +x
        export PAPERLESS_API_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/paperless-api-token")"
        export MISTRAL_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/mistral-api-key")"
        exec ${lib.getExe pkgs.paperless-gpt}
      '';
      serviceConfig = {
        DynamicUser = true;
        Group = socketGroup;
        StateDirectory = "paperless-gpt";
        StateDirectoryMode = "0700";
        WorkingDirectory = "/var/lib/paperless-gpt";
        RuntimeDirectory = "paperless-gpt";
        RuntimeDirectoryMode = "0750";
        UMask = "0007";
        Restart = "on-failure";
        RestartSec = "5s";
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        CapabilityBoundingSet = "";
        LockPersonality = true;
      };
    };
    modules.services.caddy.protectedRoutes.paperless-gpt = {
      publicHost = cfg.domain;
      upstream = "unix/${socket}";
    };
  };
}
