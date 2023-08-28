{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
  username = config.modules.users.primaryUser.username;
in {
  options.modules.desktop = {
    enable = mkBoolOpt false;
    defaultSession = mkStrOpt "hyprland"; # or "plasmawayland"
    setupCommands = mkStrOpt "";
  };
  config = mkIf cfg.enable {
    services.xserver.enable = true;
    services.xserver.displayManager = {
      defaultSession = cfg.defaultSession;
      sddm = {
        enable = true;
        settings.X11.UserAuthFile = ".local/share/sddm/Xauthority";
      };

      importedVariables = [
        "XDG_SESSION_TYPE"
        "XDG_CURRENT_DESKTOP"
        "XDG_SESSION_DESKTOP"
      ];

      setupCommands =
        ''
          export XDG_RUNTIME_DIR=/run/user/$(id --user)
          export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id --user)/bus

        ''
        + cfg.setupCommands;
    };

    #services.dbus.packages = with pkgs; [pkgs.dconf];

    environment.persistence."/persist" = mkIf config.modules.impermanence.enable {
      directories = [
        "/var/lib/sddm/.config"
      ];
      files = [
        "/var/lib/sddm/state.conf"
      ];
    };

    home-manager.users."${username}" = {
      # home-manager/#2064
      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = ["graphical-session-pre.target"];
        };
      };
    };
  };
}
