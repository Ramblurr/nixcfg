{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.flatpak;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.services.flatpak = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    environment.persistence."/persist" = { directories = [ "/var/lib/flatpak" ]; };
    # Workaround for https://github.com/NixOS/nixpkgs/issues/119433#issuecomment-1694123978
    system.fsPackages = [ pkgs.bindfs ];
    fileSystems = let
      mkRoSymBind = path: {
        device = path;
        fsType = "fuse.bindfs";
        options = [ "ro" "resolve-symlinks" "x-gvfs-hide" ];
      };
      aggregatedIcons = pkgs.buildEnv {
        name = "system-icons";
        paths = with pkgs; [
          libsForQt5.breeze-qt5 # for plasma
          gnome.gnome-themes-extra
        ];
        pathsToLink = [ "/share/icons" ];
      };
      aggregatedFonts = pkgs.buildEnv {
        name = "system-fonts";
        paths = config.fonts.packages;
        pathsToLink = [ "/share/fonts" ];
      };
    in {
      "/usr/share/icons" = mkRoSymBind "${aggregatedIcons}/share/icons";
      "/usr/local/share/fonts" = mkRoSymBind "${aggregatedFonts}/share/fonts";
    };
    systemd.user.services.flatpak-auto-update = {
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.flatpak}/bin/flatpak --user update --noninteractive --assumeye";
      };
    };

    systemd.user.timers.flatpak-auto-update = {
      enable = true;
      description = "Enable automatic flatpak updates";
      timerConfig = {
        OnCalendar = "daily";
        Persistent = "true";
      };
    };
    home-manager.users."${username}" = {
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [ ".cache/flatpak" ".local/share/flatpak" ".var/app" ];
      };
    };
  };
}
