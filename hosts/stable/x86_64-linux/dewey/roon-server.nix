{ config, fetchurl, pkgs, lib, inputs, ... }:
let
  name = "roon-server";
  user = "roon-server";
  group = "roon-server";
in {
  services.roon-server = {
    enable = true;
    openFirewall = true;
  };

  networking.firewall.allowedTCPPorts = [
    33399 # Roon ARC
  ];
  networking.firewall.allowedUDPPorts = [
    33399 # Roon ARC
  ];
  systemd.services.roon-server = {
    serviceConfig = {
      IOWeight = "200"; # default, when unspecified is 100
      OOMScoreAdjust =
        "-500"; # default, when unspecified is 0. lower means less likely to be oom-killed
      Nice = "-2"; # default, when unspecified is 0
      CPUWeight = "300"; # default, when unspecified is 100
      MemoryLow = "2G";
      MemoryHigh = "8G";
    };
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
    directories = [ "/var/lib/roon-server" ];
  };
}
