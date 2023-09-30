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
  cfg = config.modules.desktop.browsers.chromium;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.browsers.chromium = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [chromium];

    nixpkgs.config.chromium.commandLineArgs =
      mkIf config.modules.desktop.wayland.enable
      "--enable-features=UseOzonePlatform --ozone-platform=wayland";

    home-manager.users."${username}" = {pkgs, ...} @ hm: {
      home.persistence."/persist${homeDirectory}" = {
        directories = [
          ".config/chromium"
          ".cache/chromium"
        ];
      };
    };
  };
}
