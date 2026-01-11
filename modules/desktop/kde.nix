{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.kde;
  withImpermanence = config.modules.impermanence.enable;

in
{
  options.modules.desktop.kde = {
    enable = lib.mkEnableOption "";
    sddm.hideUsers = lib.mkOption {
      description = "List of users to hide from the SDDM login screen";
      default = [ ];
      example = [
        "alice"
      ];
      type = lib.types.listOf lib.types.str;
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && !config.modules.desktop.hyprland.enable;
        message = "My KDE config is mutually exclusive with hyprland";
      }
    ];
    services.displayManager.enable = true;
    services.desktopManager.plasma6.enable = true;
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      settings = {
        # convert the list of users to a string with , delimiter
        Users.HideUsers = lib.optionalString (cfg.sddm.hideUsers != [ ]) (
          lib.concatStringsSep "," cfg.sddm.hideUsers
        );
        Users.RememberLastUser = cfg.sddm.hideUsers == [ ];
        Theme.EnableAvatars = false;
        Users.MinimumUid = 99999;
        Users.MaximumUid = 99999;
      };
    };
    programs.dconf.enable = true;

    environment.systemPackages = [
      pkgs.kdePackages.krohnkite
    ];

    environment.plasma6.excludePackages = [ pkgs.kdePackages.khelpcenter ];

    environment.persistence."/persist".directories = lib.mkIf withImpermanence [
      "/var/lib/NetworkManager"
      "/etc/NetworkManager/system-connections"
    ];

    # ref: https://github.com/NixOS/nixpkgs/issues/180175
    #systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;
    #systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

    myhm = _: {
      # Make certain user services happy
      # https://github.com/nix-community/home-manager/issues/2064
      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = [ "graphical-session-pre.target" ];
        };
      };
    }; # end home manager
  };
}
