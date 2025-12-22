{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.cad;
  inherit (config.modules.users.primaryUser) username;
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
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          freecad
          # BLOCKED: https://github.com/NixOS/nixpkgs/issues/375763
          #(inputs.cadquery.packages.${pkgs.system}.cq-editor)
          openscad
          #openscad-lsp
        ];
      };
  };
}
