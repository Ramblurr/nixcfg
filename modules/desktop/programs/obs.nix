{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.obs;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.obs = {
    enable = lib.mkEnableOption "";
    cudaSupport = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

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

    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        programs.obs-studio = {
          enable = true;
          package =
            if cfg.cudaSupport then
              (pkgs.obs-studio.override {
                cudaSupport = true;
              })
            else
              pkgs.obs-studio;

          # TODO: is this even needed? isn't it built in?
          plugins = with pkgs; [
            # obs-studio-plugins.wlrobs
          ];
        };
        home.persistence."/persist" = mkIf withImpermanence {
          directories = [
            ".config/obs-studio"
          ];
        };
      };
  };
}
