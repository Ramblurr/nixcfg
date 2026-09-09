{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.flatpak;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.services.flatpak = {
    enable = lib.mkEnableOption "";
    autoUpdate.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Define the existing daily user Flatpak update service and timer.";
    };
    userFlathub.enable = lib.mkEnableOption "Flathub in the primary user's Flatpak installation";
  };
  config = lib.mkIf cfg.enable {
    services.flatpak.enable = true;
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      directories = [ "/var/lib/flatpak" ];
    };
    # Workaround for https://github.com/NixOS/nixpkgs/issues/119433#issuecomment-1694123978
    system.fsPackages = [ pkgs.bindfs ];
    fileSystems =
      let
        mkRoSymBind = path: {
          device = path;
          fsType = "fuse.bindfs";
          options = [
            "ro"
            "resolve-symlinks"
            "x-gvfs-hide"
          ];
        };
        aggregatedIcons = pkgs.buildEnv {
          name = "system-icons";
          paths = [ pkgs.gnome-themes-extra ];
          pathsToLink = [ "/share/icons" ];
        };
        aggregatedFonts = pkgs.buildEnv {
          name = "system-fonts";
          paths = config.fonts.packages;
          pathsToLink = [ "/share/fonts" ];
        };
      in
      {
        "/usr/share/icons" = mkRoSymBind "${aggregatedIcons}/share/icons";
        "/usr/local/share/fonts" = mkRoSymBind "${aggregatedFonts}/share/fonts";
      };
    systemd.user.services.flatpak-auto-update = lib.mkIf cfg.autoUpdate.enable {
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.flatpak}/bin/flatpak --user update --noninteractive --assumeye";
      };
    };

    systemd.user.timers.flatpak-auto-update = lib.mkIf cfg.autoUpdate.enable {
      enable = true;
      description = "Enable automatic flatpak updates";
      timerConfig = {
        OnCalendar = "daily";
        Persistent = "true";
      };
    };

    # Run as the primary user, retrying when first login happens offline.
    myhm.systemd.user.services.flathub-user = lib.mkIf cfg.userFlathub.enable {
      Unit = {
        Description = "Initialize user-scoped Flathub";
        StartLimitIntervalSec = 0;
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.flatpak}/bin/flatpak remote-add --user --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo";
        RemainAfterExit = true;
        Restart = "on-failure";
        RestartSec = 30;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
