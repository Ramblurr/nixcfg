{
  lib,
  inputs,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-guest;
  username = cfg.homeManager.username;
  uid = cfg.homeManager.uid;
  home = config.users.users.${username}.home;
in
lib.mkIf cfg.homeManager.enable {
  nix.settings.allowed-users = [ username ];
  users.users.${username} = {
    name = username;
    isNormalUser = true;
    uid = uid;
    group = username;
    linger = true;
    home = "/home/${username}";
    createHome = true;
    autoSubUidGidRange = true;
  };

  users.groups.${username} = {
    name = username;
    gid = cfg.homeManager.gid;
  };

  systemd.services."home-manager-${username}" = {
    serviceConfig.TimeoutStartSec = lib.mkOverride 99 "15m";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };
  home-manager.users.${username} =
    { pkgs, config, ... }@hm:
    {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.sops-nix.homeManagerModule
        (lib.mkAliasOptionModule
          [ "persistence" ]
          [
            "home"
            "persistence"
            "/persist${username}"
          ]
        )
      ];
      home.homeDirectory = home;
      home.sessionVariables = {
        EDITOR = "vim";
        DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString uid}/bus";
        XDG_RUNTIME_DIR = "/run/user/${toString uid}";
      };
      systemd.user.startServices = "sd-switch";
      programs.bash = {
        enable = true;
        initExtra = ''
          [[ -f "${hm.config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${hm.config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
        '';
      };
    };
}
