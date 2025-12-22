{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.logseq;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.logseq = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Logseq"
          ".logseq"
        ];
      };
    };

    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        # I am using flatpak logseq, because it is more stable than the nix pkg (as of 2024-09)
        # ref: https://github.com/NixOS/nixpkgs/issues/264885
        #      https://github.com/logseq/logseq/issues/10851
        home.packages = [ pkgs.logseq ];
        #Exec=env -u NIXOS_OZONE_WL logseq %u
        #Icon=logseq
        home.file.".local/share/applications/logseq-wayland.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Logseq (flatpak wayland)
            Exec=GDK_BACKEND=wayland flatpak run com.logseq.Logseq --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WaylandWindowDecorations %u
            Terminal=false
            Type=Application
            Icon=com.logseq.Logseq
            StartupWMClass=Logseq
            Comment=A privacy-first, open-source platform for knowledge management and collaboration.
            MimeType=x-scheme-handler/logseq
            Categories=Utility
          '';
        };
      };
  };
}
