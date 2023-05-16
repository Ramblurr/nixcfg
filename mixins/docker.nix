{config, ...}: {
  config = {
    environment.persistence."/persist" = {
      directories = [
        "/var/lib/docker"
      ];
    };
    virtualisation.docker = {
      enable = true;
      #storageDriver = "zfs";

      # we don't use long running docker containers, start on socket
      enableOnBoot = false;
    };
  };
}
