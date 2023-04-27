{
  config,
  lib,
  pkgs,
  ...
}: {
  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
    enableExcludeWrapper = true;
  };
  environment.persistence = {
    "/persist".directories = [
      "/etc/mullvad-vpn"
      "/var/cache/mullvad-vpn"
    ];
  };

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
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
