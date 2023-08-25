{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    lib,
    ...
  }: {
    services.nextcloud-client = {
      enable = true;
      startInBackground = true;
    };

    home.persistence = {
      "/persist/home/ramblurr".directories = [
        ".config/Nextcloud"
        ".local/share/Nextcloud"
      ];
    };
  };
}
