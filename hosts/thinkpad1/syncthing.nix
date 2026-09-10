{ config, lib, ... }:
let
  user = config.modules.users.primaryUser.username;
  home = config.modules.users.primaryUser.homeDirectory;
  restored = config.repo.secrets.local.syncthing;
in
{
  sops.secrets.syncthing-key.owner = user;
  sops.secrets.syncthing-cert.owner = user;

  services.syncthing = {
    enable = true;
    systemService = true;
    inherit user;
    group = config.users.users.${user}.group;
    dataDir = "${home}/.config/syncthing";
    configDir = "${home}/.config/syncthing";
    guiAddress = "127.0.0.1:8384";
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;
    key = config.sops.secrets.syncthing-key.path;
    cert = config.sops.secrets.syncthing-cert.path;
    settings = {
      devices = restored.devices;
      folders = lib.mapAttrs (
        _: folder:
        folder
        // {
          path = "${home}/Sync/${folder.label}";
        }
      ) restored.folders;
      defaults.folder.path = "${home}/Sync";
    };
  };

  # Reuse the encrypted identity and only the explicitly configured shares.
  systemd.services.syncthing = {
    after = [ "home-manager-${user}.service" ];
    environment.STNODEFAULTFOLDER = "true";
  };
  systemd.services.syncthing-init.after = [ "home-manager-${user}.service" ];
}
