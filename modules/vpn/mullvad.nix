{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.vpn.mullvad;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
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

          #${mullvad}/bin/mullvad lan set allow
          #${mullvad}/bin/mullvad relay set tunnel-protocol wireguard
          #${mullvad}/bin/mullvad auto-connect set on
          #${mullvad}/bin/mullvad tunnel wireguard quantum-resistant-tunnel set auto
          ## previously known as always-require-vpn
          #${mullvad}/bin/mullvad lockdown-mode set on
          #${mullvad}/bin/mullvad dns set default --block-ads --block-malware --block-trackers
        '';
    };

    environment.persistence = mkIf (cfg.enable && config.modules.impermanence.enable) {
      "/persist".directories = [
        "/etc/mullvad-vpn"
        "/var/cache/mullvad-vpn"
      ];
    };

    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      mkIf cfg.enable {
        home.persistence."/persist${homeDirectory}" = mkIf config.modules.impermanence.enable {
          directories = [
            {
              method = "symlink";
              directory = ".config/Mullvad VPN";
            }
          ];
        };
      };
  };
}
