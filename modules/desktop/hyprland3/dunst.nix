{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
  username = config.modules.users.primaryUser.username;
  eitherStrBoolIntList = with lib.types; either str (either bool (either int (listOf str)));
  toDunstIni = lib.generators.toINI {
    mkKeyValue =
      key: value:
      let
        value' =
          if lib.isBool value then
            (if value then "yes" else "no")
          else if lib.isString value then
            ''"${value}"''
          else
            toString value;
      in
      "${key}=${value'}";
  };
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      { pkgs, ... }@hm:
      {
        systemd.user.services.dunst = {
          Unit = {
            Description = "Dunst notification daemon";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "dbus";
            BusName = "org.freedesktop.Notifications";
            ExecStart = "${pkgs.dunst}/bin/dunst -config ${hm.config.xdg.configHome}/dunst/dunstrc";
          };
        };

        xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source = "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
        xdg.configFile."dunst/dunstrc" = {
          text = toDunstIni (
            lib.my.mergeAttrs' [
              {
                global = {
                  title = "Dunst";
                  class = "Dunst";
                  browser = "firefox-nightly -new-tab";
                  #dmenu = "rofi -dmenu -p dunst";
                  history_length = 20;
                  idle_threshold = 120;
                  markup = "full";
                  sort = true;
                  sticky_history = true;
                  progress_bar = true;
                  indicate_hidden = true;
                  notification_limit = 0;
                  monitor = 0;
                };
                experimental = {
                  enable_recursive_icon_lookup = true;
                };
              }
              #cfg.settings
            ]
          );
          onChange = ''
            ${pkgs.procps}/bin/pkill -u "$USER" ''${VERBOSE+-e} dunst || true
          '';
        };
      };
  };
}
