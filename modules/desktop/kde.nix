{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.desktop.kde;
  withImpermanence = config.modules.impermanence.enable;

in
{
  options.modules.desktop.kde = {
    enable = lib.mkEnableOption "";
    sddm.listUsers = lib.mkEnableOption "listing normal user accounts on the login screen";
    krohnkite.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install the optional Krohnkite tiling extension.";
    };
    sddm.hideUsers = lib.mkOption {
      description = "List of users to hide from the SDDM login screen";
      default = [ ];
      example = [
        "alice"
      ];
      type = lib.types.listOf lib.types.str;
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && !config.modules.desktop.niri.enable;
        message = "My KDE config is mutually exclusive with niri";
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
        Users.MinimumUid = if cfg.sddm.listUsers then 1000 else 99999;
        Users.MaximumUid = if cfg.sddm.listUsers then 60000 else 99999;
      };
    };
    programs.dconf.enable = true;

    environment.systemPackages = lib.optional cfg.krohnkite.enable pkgs.kdePackages.krohnkite;

    environment.plasma6.excludePackages = [ pkgs.kdePackages.khelpcenter ];

    environment.persistence = lib.mkIf withImpermanence {
      "/persist".directories = [
        "/var/lib/NetworkManager"
        "/etc/NetworkManager/system-connections"
      ];
    };

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
