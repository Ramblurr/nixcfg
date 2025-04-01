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
  inherit (config.users.users.linkding) uid home;
in
lib.mkIf cfg.quadlet.enable {
  virtualisation.podman = {
    enable = true;
    autoPrune.enable = true;
    autoPrune.dates = "weekly";
    #defaultNetwork.settings.dns_enabled = true;
  };
  home-manager.users.${username} =
    { pkgs, config, ... }:
    {
      virtualisation.quadlet.autoUpdate.enable = lib.mkDefault true;
      home.packages = [
        pkgs.podman
        pkgs.nix
        pkgs.dive
        pkgs.podman-tui
      ];
      #xdg.configFile."containers/containers.conf" = {
      #  text = '''';
      #};
      xdg.configFile."containers/storage.conf" = {
        text = ''
          [storage]
          driver = "overlay"
          rootless_storage_path="/var/lib/podman/${username}/containers/storage"
        '';
      };

      xdg.configFile."systemd/user/podman-user-wait-network-online.service.d/override2.conf" = {
        text = ''
          [Service]
          ExecSearchPath=/run/current-system/sw/bin
        '';
      };
    };
}
