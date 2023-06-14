{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.enable = true;
  services.xserver.displayManager = {
    defaultSession = "plasmawayland";
    sddm = {
      enable = true;
      settings.X11.UserAuthFile = ".local/share/sddm/Xauthority";
    };

    importedVariables = [
      "XDG_SESSION_TYPE"
      "XDG_CURRENT_DESKTOP"
      "XDG_SESSION_DESKTOP"
    ];

    setupCommands = ''
      export XDG_RUNTIME_DIR=/run/user/$(id --user)
      export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id --user)/bus

      xrandr \
        --output DP-1 --mode 5120x1440 --auto
      xrandr \
        --output HDMI-A-1 --mode 3840x2160 --auto
    '';
  };

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/sddm/.config"
    ];
    files = [
      "/var/lib/sddm/state.conf"
    ];
  };
}
