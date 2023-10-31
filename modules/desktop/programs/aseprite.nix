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
  cfg = config.modules.desktop.programs.aseprite;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.aseprite = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    myhm = {
      home.packages = with pkgs; [
        (pkgs.aseprite.overrideAttrs (attrs: {
          version = "main";
          src = fetchFromGitHub {
            owner = "aseprite";
            repo = "aseprite";
            rev = "fc29686bb204f07f02729de0756d8fcb15e08de7";
            fetchSubmodules = true;
            hash = "sha256-c2b9Aop5lk93Wz+qFzz8V5qyjZX/brXpQrAPS42b6ZE=";
          };
          postPatch = "";
        }))
      ];
      persistence = mkIf withImpermanence {
        directories = [
          ".config/libresprite"
          ".config/aseprite"
        ];
      };
    };
  };
}
