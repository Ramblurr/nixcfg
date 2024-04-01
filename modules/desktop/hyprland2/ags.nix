{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland2;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in

{
  config = lib.mkIf cfg.enable {

    myhm =
      { ... }@hm:
      {
        home.packages = with pkgs; [
          icon-library
          gnome.dconf-editor
          inputs.matugen.packages.${system}.default
          cfg.ags-config
          bun
          dart-sass
          fd
          brightnessctl
          swww
          slurp
          wf-recorder
          wl-clipboard
          wayshot
          satty
          hyprpicker
          pavucontrol
          networkmanager
          gtk3
        ];
        programs.ags = {
          enable = true;
          configDir = ../../../configs/ags;
          extraPackages = with pkgs; [ accountsservice ];
        };

        persistence = lib.mkIf withImpermanence {
          directories = [
            ".cache/ags"
            #".config/ags"
          ];
        };
      };

    #systemd.tmpfiles.rules = lib.mkIf withImpermanence [
    #  "d /persist${homeDirectory}/.config/ags 700 ${username} ${username}"
    #  "d /persist${homeDirectory}/.cache/age 700 ${username} ${username}"
    #];
  };
}
