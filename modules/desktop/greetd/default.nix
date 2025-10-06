{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.greetd;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.greetd = {
    enable = lib.mkEnableOption "Enable Greetd";
  };

  config = mkIf cfg.enable {

    security = {
      polkit.enable = true;
      # unlock keyring on login
      pam.services.greetd.enableGnomeKeyring = true;
    };

    systemd.tmpfiles.rules = lib.mkIf withImpermanence [
      "d /persist/var/cache/greeter - greeter greeter - -"
    ];

    environment.persistence."/persist".directories = lib.mkIf withImpermanence [ "/var/cache/greeter" ];
    services.greetd = {
      enable = true;
      settings = rec {
        terminal = {
          vt = lib.mkForce 7;
        };
        tuigreet_session =
          let
            session = "${pkgs.hyprland}/bin/Hyprland";
            tuigreet = "${lib.getExe pkgs.tuigreet}";
            opts = [
              "--time"
              "--remember"
              "--theme"
              "border=lightblue;prompt=green;time=orange;button=yellow;container=black"
              "--power-shutdown"
              "systemctl poweroff -i"
              "--power-reboot"
              "systemctl reboot -i"
              "--cmd"
              "${session}"
            ];
          in
          {
            command = "${tuigreet} ${lib.strings.escapeShellArgs opts}";
            user = "greeter";
          };
        default_session = tuigreet_session;
      };
    };

    # allow greeter to poweroff and reboot
    security.polkit.extraConfig = ''
       polkit.addRule(function(action, subject) {
      if (
        subject.user == "greeter"
          && (
            action.id == "org.freedesktop.login1.reboot" ||
            action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
            action.id == "org.freedesktop.login1.power-off" ||
            action.id == "org.freedesktop.login1.power-off-multiple-sessions"
          )
        )
      {
        return polkit.Result.YES;
      }
       })
    '';
  };
}
