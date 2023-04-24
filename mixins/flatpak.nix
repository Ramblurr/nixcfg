{pkgs, ...}: {
  config = {
    services.flatpak.enable = true;

    environment.persistence."/persist" = {
      directories = [
        "/var/lib/flatpak"
      ];
    };
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".cache/flatpak"
          ".local/share/flatpak"
        ];
      };
    };
  };
}
