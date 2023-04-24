{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 1714;
      to = 1764;
    }
  ];
  networking.firewall.allowedUDPPortRanges = [
    {
      from = 1714;
      to = 1764;
    }
  ];
  home-manager.users.ramblurr = {
    pkgs,
    lib,
    ...
  }: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/kdeconnect"
      ];
    };

    services.kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
}
