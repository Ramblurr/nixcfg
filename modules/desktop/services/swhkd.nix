{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.services.swhkd;

  keybindingsStr = concatStringsSep "\n" (
    mapAttrsToList (
      hotkey: command:
      optionalString (command != null) ''
        ${hotkey}
          ${command}
      ''
    ) cfg.keybindings
  );

  configStr = concatStringsSep "\n" [
    keybindingsStr
    cfg.extraConfig
  ];

  devicesStr = concatMapStringsSep " " (x: "--device ${x}") (cfg.devices or [ ]);

  configFile = pkgs.writeText "swhkdrc" configStr;
in
{

  options.modules.desktop.services.swhkd = {
    # you must run swhks manually
    enable = mkEnableOption "swhkd";
    package = mkOption {
      type = types.package;
      default = pkgs.swhkd;
      description = ''
        swhkd package to use
      '';
    };
    keybindings = mkOption {
      type = types.attrsOf (types.nullOr types.str);
      default = { };
      description = "An attribute set that assigns hotkeys to commands.";
      example = literalExpression ''
        {
          "super + shift + {r,c}" = "i3-msg {restart,reload}";
          "super + {s,w}"         = "i3-msg {stacking,tabbed}";
        }
      '';
    };

    cooldown = mkOption {
      type = types.int;
      default = 250;
      description = ''
        Set a custom repeat cooldown duration. Default is 250ms. Most wayland compositors handle this server side however, either way works.
      '';
    };

    debug = mkEnableOption "swhkd debug mode";

    devices = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
      description = ''
        Set the keyboard devices to use.
      '';
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      description = "Additional configuration to add.";
      example = literalExpression ''
        super + {_,shift +} {1-9,0}
          i3-msg {workspace,move container to workspace} {1-10}
      '';
    };
  };
  config = mkIf cfg.enable {
    myhm =
      { ... }@hm:
      {
        home.packages = [ cfg.package ];
        xdg.configFile."swhkd/swhkdrc".source = configFile;
      };
    security.polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
            if (action.id == "com.github.swhkd.pkexec"  &&
                subject.local == true &&
                subject.active == true &&) {
                    return polkit.Result.YES;
                }
        });
      '';
    };

    systemd.user.services.swhkd = {
      description = "swhkd hotkey daemon";
      #bindsTo = [ "default.target" ];
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session-pre.target" ];

      wantedBy = [ "graphical-session.target" ];
      script = ''
        /run/wrappers/bin/pkexec ${cfg.package}/bin/swhkd \
          --config $XDG_CONFIG_HOME/swhkd/swhkdrc \
          --cooldown ${toString cfg.cooldown} \
          ${optionalString (cfg.devices != null) devicesStr} \
          ${optionalString cfg.debug "--debug"}
      '';
      # --config ${configFile} \
      restartTriggers = [ configFile ];
      serviceConfig = {
        Restart = "always";
        Type = "simple";
      };
    };
  };
}
