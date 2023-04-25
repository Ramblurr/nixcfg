{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/sddm/.config"
    ];
    files = [
      "/var/lib/sddm/state.conf"
    ];
  };
}
