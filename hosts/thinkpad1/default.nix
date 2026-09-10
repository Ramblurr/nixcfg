{ lib, pkgs, ... }:
{
  imports = [
    ../../config/common.nix
    ./hardware.nix
  ];

  system.stateVersion = "26.05";

  modules = {
    users.enable = true;
    desktop.programs.onepassword.enable = true;
    desktop.kde = {
      enable = true;
      krohnkite.enable = false;
    };
    services = {
      flatpak = {
        enable = true;
        autoUpdate.enable = false;
        userFlathub.enable = true;
      };
      sshd = {
        enable = true;
        openFirewall = false;
      };
    };
    vpn.tailscale.enable = true;
    firewall.enable = true;
    security.default.enable = true;
    hardware.pipewire.enable = true;
  };

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;
  services = {
    timesyncd.enable = true;
    fwupd.enable = true;
    power-profiles-daemon.enable = true;
    displayManager = {
      sddm.enable = lib.mkForce false;
      plasma-login-manager = {
        enable = true;
        settings.Greeter.PreselectedUser = "viki";
      };
    };
  };

  # Plasma supplies its portals, Dolphin, Discover, and ordinary desktop utilities.
  # Avoid the shared XDG module's personal directory layout and browser profiles.
  xdg.portal.xdgOpenUsePortal = true;
  programs = {
    firefox.enable = true;
    kde-pim.enable = false;
    kdeconnect.enable = true;
    appimage = {
      enable = true;
      binfmt = true;
    };
  };
  fonts.packages = [
    pkgs.noto-fonts
    pkgs.noto-fonts-color-emoji
    pkgs.dejavu_fonts
  ];

  boot.plymouth = {
    enable = true;
    theme = "catppuccin-mocha";
    themePackages = [ (pkgs.catppuccin-plymouth.override { variant = "mocha"; }) ];
  };
  environment = {
    # Ordinary optional applications belong to the user's Flathub installation.
    plasma6.excludePackages = [
      pkgs.kdePackages.elisa
      pkgs.kdePackages.kate
      pkgs.kdePackages.ktexteditor
      pkgs.kdePackages.okular
      pkgs.kdePackages.gwenview
    ];
    systemPackages = [
      (pkgs.catppuccin-kde.override {
        flavour = [ "mocha" ];
        accents = [ "mauve" ];
      })
    ];
    # System defaults only: ~/.config/kdeglobals remains owned by the user.
    etc."xdg/kdeglobals".text = ''
      [General]
      ColorScheme=CatppuccinMochaMauve
    '';
  };
}
