{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain)
    personal2
    ;
  username = personal2;
  homeDir = "/var/lib/${username}";
  uid = config.users.users.${personal2}.uid;
in
{

  users.users.${username} = {
    isSystemUser = true;
    shell = pkgs.bash;
    linger = true;
    home = homeDir;
    createHome = true;
    group = username;
  };

  users.groups.${username} = {
    name = username;
  };

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/${personal2}"."mountpoint" = homeDir;
  };

  home-manager.users.${username} =
    { pkgs, config, ... }:
    {
      home.homeDirectory = homeDir;
      home.sessionVariables = {
        EDITOR = "vim";
        DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString uid}/bus";
        XDG_RUNTIME_DIR = "/run/user/${toString uid}";
      };

      systemd.user.startServices = "sd-switch";
      programs.bash = {
        enable = true;
        initExtra = ''
          [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        '';
      };
    };
}
