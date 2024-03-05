{
  config,
  fetchurl,
  pkgs,
  lib,
  inputs,
  ...
}: let
  name = "roon-server";
  user = "roon-server";
  group = "roon-server";
in {
  services.roon-server = {
    enable = true;
    openFirewall = true;
  };
  fileSystems."/mnt/roon/backup" = {
    device = "10.9.10.10:/mnt/tank2/backups/roon";
    fsType = "nfs";
  };
  fileSystems."/mnt/roon/music-other" = {
    device = "10.9.10.10:/mnt/tank2/media/music/other";
    fsType = "nfs";
  };
  fileSystems."/mnt/roon/music-mine" = {
    device = "10.9.10.10:/mnt/tank2/media/music/mine";
    fsType = "nfs";
  };
  fileSystems."/mnt/roon/audiobooks" = {
    device = "10.9.10.10:/mnt/tank2/media/audiobooks";
    fsType = "nfs";
  };
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/roon-server"
    ];
  };
}
