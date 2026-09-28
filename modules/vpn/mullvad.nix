{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.vpn.mullvad;
in
{
  options.modules.vpn.mullvad = {
    enable = lib.mkEnableOption "";
    autoLogin.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Automatically log in using the SOPS-managed Mullvad account.";
    };
  };
  config = mkIf cfg.enable {
    services.mullvad-vpn = mkIf cfg.enable {
      enable = true;
      gui.enable = true;
      enableExcludeWrapper = true;
    };

    sops.secrets.mullvad-account = lib.mkIf cfg.autoLogin.enable { };
    systemd.services.mullvad-daemon = lib.mkIf cfg.autoLogin.enable {
      serviceConfig.LoadCredential = [ "account:${config.sops.secrets.mullvad-account.path}" ];
      postStart =
        let
          mullvad = config.services.mullvad-vpn.package;
        in
        ''
          #!/bin/sh
          while ! ${mullvad}/bin/mullvad status &>/dev/null; do sleep 1; done
          account="$(<"$CREDENTIALS_DIRECTORY/account")"
          current_account="$(${mullvad}/bin/mullvad account get | grep "account:" | sed 's/.* //')"
          if [[ "$current_account" != "$account" ]]; then
            ${mullvad}/bin/mullvad account login "$account"
          fi
        '';
    };

    environment.persistence = mkIf (cfg.enable && config.modules.impermanence.enable) {
      "/persist".directories = [
        "/etc/mullvad-vpn"
        "/var/cache/mullvad-vpn"
      ];
    };

  };
}
