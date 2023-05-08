{
  config,
  lib,
  pkgs,
  ...
}: {
  services.mullvad-vpn = lib.mkIf config.deviceSpecific.vpn.mullvad.enable {
    enable = true;
    package = pkgs.mullvad-vpn;
    enableExcludeWrapper = true;
  };

  sops.secrets.mullvad-account = {};
  systemd.services.mullvad-daemon = {
    serviceConfig.LoadCredential = ["account:${config.sops.secrets.mullvad-account.path}"];
    postStart = let
      mullvad = config.services.mullvad-vpn.package;
    in ''
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

  environment.persistence = lib.mkIf config.deviceSpecific.vpn.mullvad.enable {
    "/persist".directories = [
      "/etc/mullvad-vpn"
      "/var/cache/mullvad-vpn"
    ];
  };

  home-manager.users.ramblurr = {pkgs, ...} @ hm:
    lib.mkIf config.deviceSpecific.vpn.mullvad.enable {
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          {
            method = "symlink";
            directory = ".config/Mullvad VPN";
          }
        ];
      };
    };
}
