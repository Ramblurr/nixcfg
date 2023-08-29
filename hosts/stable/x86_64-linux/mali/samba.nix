{
  config,
  lib,
  pkgs,
  ...
}: {
  services.samba-wsdd.enable = true;
  services.samba-wsdd.workgroup = "WORKGROUP";
  networking.firewall.allowedTCPPorts = [
    5357 # wsdd
  ];
  networking.firewall.allowedUDPPorts = [
    3702 # wsdd
  ];
  services.samba = {
    enable = true;
    securityType = "user";
    # enableNmbd = false;
    # enableWinbindd = false;
    openFirewall = true;
    extraConfig = ''
      workgroup = WORKGROUP
      browseable = yes
      smb encrypt = required
      load printers = no
      printcap name = /dev/null
      guest account = nobody
      map to guest = bad user
    '';

    shares = {
      roon = {
        path = "/mnt/tank2/backups/roon";
        browseable = "yes";
        "force user" = "roon";
        "guest ok" = "no";
        public = "no";
        "read only" = "no";
        writeable = "yes";
      };
      music-mine = {
        path = "/mnt/tank2/media/music/mine";
        browseable = "yes";
        "force user" = "roon";
        "guest ok" = "no";
        "read only" = "no";
        writeable = "no";
      };
      music-other = {
        path = "/mnt/tank2/media/music/other";
        browseable = "yes";
        "force user" = "roon";
        "guest ok" = "no";
        "read only" = "no";
        writeable = "no";
      };
    };
  };
}
