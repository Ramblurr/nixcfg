{ config, ... }:

let
  environmentFile = "/run/gatus-env/gatus.env";
  onepassword = config.modules.services.onepassword-systemd-credentials;
in
{
  assertions = [
    {
      assertion = onepassword.enable;
      message = "Gatus Pushover credentials require the 1Password systemd credential provider.";
    }
  ];

  modules.services.onepassword-systemd-credentials.consumers.gatus-env-setup = {
    pushover-api-token = "op://home-ops-prod/pushover/pushover_api_token";
    pushover-user-key = "op://home-ops-prod/pushover/pushover_user_key";
  };

  systemd.services.gatus-env-setup = {
    description = "Prepare Gatus environment from 1Password credentials";
    before = [ "gatus.service" ];
    requiredBy = [ "gatus.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      RuntimeDirectory = "gatus-env";
      UMask = "0077";
    };
    script = ''
      {
        printf 'PUSHOVER_API_TOKEN=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/pushover-api-token")"
        printf 'PUSHOVER_USER_KEY=%s\n' "$(cat "$CREDENTIALS_DIRECTORY/pushover-user-key")"
      } > ${environmentFile}
    '';
  };

  services.gatus = {
    inherit environmentFile;
    settings.alerting.pushover = {
      "application-token" = "$PUSHOVER_API_TOKEN";
      "user-key" = "$PUSHOVER_USER_KEY";
    };
  };
}
