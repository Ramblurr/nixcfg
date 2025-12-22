{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.greetd;
  withImpermanence = config.modules.impermanence.enable;

  #runHyprland = runViaShell {
  #  env = {
  #    XDG_SESSION_TYPE = "wayland";
  #    XDG_CURRENT_DESKTOP = "Hyprland";
  #    XDG_SESSION_DESKTOP = "Hyprland";
  #  };
  #  name = "hyprland";
  #  cmd = "${pkgs.hyprland}/bin/Hyprland";
  #};

  #runSway = runViaShell {
  #  env = {
  #    XDG_SESSION_TYPE = "wayland";
  #    XDG_CURRENT_DESKTOP = "sway";
  #    XDG_SESSION_DESKTOP = "sway";
  #  };
  #  name = "sway";
  #  cmd = "${pkgs.swayfx}/bin/sway";
  #};

  #runRiver = runViaShell {
  #  env = {
  #    XDG_SESSION_TYPE = "wayland";
  #    XDG_CURRENT_DESKTOP = "river";
  #    XDG_SESSION_DESKTOP = "river";
  #  };
  #  name = "river";
  #  cmd = "${pkgs.river-classic}/bin/river";
  #};

  desktopSession =
    name: command:
    pkgs.writeText "${name}.desktop" ''
      [Desktop Entry]
      Type=Application
      Name=${name}
      Exec=${command}
    '';

  sessions = [
    # {
    #   name = "sway.desktop";
    #   path = desktopSession "sway" "${runSway}/bin/sway";
    # }
    # {
    #   name = "river.desktop";
    #   path = desktopSession "river" "${runRiver}/bin/river";
    # }
    #{
    #  name = "hyprland.desktop";
    #  path = desktopSession "Hyprland" "${runHyprland}/bin/hyprland";
    #}
    #{
    #  name = "nushell.desktop";
    #  path = desktopSession "nushell" "${pkgs.nushell}/bin/nu";
    #}
    {
      name = "bash.desktop";
      path = desktopSession "bash" "${pkgs.bashInteractive}/bin/bash";
    }
    {
      name = "niri.desktop";
      path = desktopSession "niri" "${pkgs.niri}/bin/niri-session";
    }
  ];

  createGreeter =
    default: sessions:
    let
      sessionDir = pkgs.linkFarm "sessions" (
        builtins.filter (item: item.name != "${default}.desktop") sessions
      );
    in
    pkgs.writeShellApplication {
      name = "greeter";
      runtimeInputs = [
        #runSway
        #runRiver
        #runHyprland
        pkgs.bashInteractive
        #pkgs.nushell
        pkgs.systemd
        pkgs.tuigreet
      ];
      text = ''
        tuigreet --sessions ${sessionDir} --time -r --remember-session --power-shutdown 'systemctl poweroff' --power-reboot 'systemctl reboot' --cmd ${default}
      '';
    };
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
      restart = true;
      settings = {
        default_session.command = "${createGreeter "${pkgs.niri}/bin/niri-session" sessions}/bin/greeter";
        #default_session.command = "${createGreeter "${runHyprland}/bin/hyprland" sessions}/bin/greeter";
      };
    };
    ## prevents systemd spewing the console with log messages when greeter is active
    systemd.services.greetd.serviceConfig = {
      ExecStartPre = "${pkgs.util-linux}/bin/kill -SIGRTMIN+21 1";
      ExecStopPost = "${pkgs.util-linux}/bin/kill -SIGRTMIN+20 1";
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
