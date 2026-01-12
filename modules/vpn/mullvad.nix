{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.vpn.mullvad;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.vpn.mullvad = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    services.mullvad-vpn = mkIf cfg.enable {
      enable = true;
      package = pkgs.mullvad-vpn;
      enableExcludeWrapper = true;
    };

    sops.secrets.mullvad-account = { };
    systemd.services.mullvad-daemon = {
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
            ${pkgs.mullvad}/bin/mullvad account login "$account"
          fi
        '';
    };

    environment.persistence = mkIf (cfg.enable && config.modules.impermanence.enable) {
      "/persist".directories = [
        "/etc/mullvad-vpn"
        "/var/cache/mullvad-vpn"
      ];
    };

    home-manager.users."${username}" =
      _:
      mkIf cfg.enable {
        home.persistence."/persist" = mkIf config.modules.impermanence.enable {
          directories = [
            ".config/Mullvad VPN"
          ];
        };
      };
  };
}
