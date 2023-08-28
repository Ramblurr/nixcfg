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
  cfg = config.modules.desktop.programs.obs;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.obs = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    boot.extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback
    ];

    environment.systemPackages = with pkgs; [
      v4l-utils

      (pkgs.writeScriptBin "obs-v4l2loopback-setup.sh" ''
        set -x
        sudo modprobe \
          v4l2loopback \
            devices=1 \
            video_nr=13 \
            card_label="''${CAMERA_NAME:-"OBS Virtual Camera"}" \
            exclusive_caps=1
      '')
    ];

    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      programs.obs-studio = {
        enable = true;

        # TODO: is this even needed? isn't it built in?
        plugins = with pkgs; [
          # obs-studio-plugins.wlrobs
        ];
      };
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
        ];
      };
    };
  };
}
