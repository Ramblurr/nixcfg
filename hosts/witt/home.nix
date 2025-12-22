{
  pkgs,
  ...
}:

{
  #home-manager.sharedModules = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];
  myhm = _: {
    # Starship prompt color - something very different to distinguish it from my main workstation
    programs.starship.settings = pkgs.lib.importTOML ./gruvbox-rainbow.toml;

    # Make certain user services happy
    # https://github.com/nix-community/home-manager/issues/2064
    systemd.user.targets.tray = {
      Unit = {
        Description = "Home Manager System Tray";
        Requires = [ "graphical-session-pre.target" ];
      };
    };

    # Apps, apps, apps
    home.packages = with pkgs; [
      appimage-run
      musescore
      audacity
      pavucontrol
      brightnessctl
      meld
      gimp
      pdfarranger
      qpwgraph # pipewire wiring gui tool
      easyeffects # pipewire eq
    ];

  }; # end home manager
}
