{ config, ... }:
let
  user = config.modules.users.primaryUser.username;
  home = config.modules.users.primaryUser.homeDirectory;
in
{
  services.syncthing = {
    enable = true;
    systemService = true;
    inherit user;
    group = config.users.users.${user}.group;
    dataDir = home;
    configDir = "${home}/.config/syncthing";
    guiAddress = "127.0.0.1:8384";
    openDefaultPorts = true;
    overrideDevices = false;
    overrideFolders = false;
  };

  # Generate a new identity, but no default shared folder, on first startup.
  systemd.services.syncthing = {
    after = [ "home-manager-${user}.service" ];
    environment.STNODEFAULTFOLDER = "true";
  };
  systemd.services.syncthing-init.after = [ "home-manager-${user}.service" ];
}
