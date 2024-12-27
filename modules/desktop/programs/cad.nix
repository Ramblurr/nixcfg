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
  cfg = config.modules.desktop.programs.cad;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.cad = {
    enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {

    environment.persistence."/persist" = lib.mkIf withImpermanence {

      users.${username} = {
        directories = [
          ".config/FreeCAD"
          ".cache/FreeCAD"
          ".local/share/FreeCAD"
          ".config/OpenSCAD"
          ".local/share/OpenSCAD"
        ];
        files = [
          ".config/CQ-editorrc"
          ".config/OpenSCADrc"
        ];

      };
    };
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = with pkgs; [
          freecad
          (inputs.cadquery.packages.${pkgs.system}.cq-editor)
          openscad-unstable
          openscad-lsp
        ];
      };
  };
}
