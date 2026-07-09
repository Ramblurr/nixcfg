{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.cad;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.desktop.programs.cad = {
    enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {

    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          #freecad
          # BLOCKED: https://github.com/NixOS/nixpkgs/issues/375763
          #(inputs.cadquery.packages.${pkgs.stdenv.hostPlatform.system}.cq-editor)
          openscad-unstable
          #openscad-lsp
        ];
      };
  };
}
