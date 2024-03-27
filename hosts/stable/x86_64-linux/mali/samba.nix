{ config, lib, pkgs, ... }: {
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
    enableNmbd = true; # namespace and browsing suport
    enableWinbindd = true; # integrations linux user auth
    openFirewall = true;
    extraConfig = ''
      server string = smbnix
      netbios name = smbnix
      workgroup = WORKGROUP
      browseable = yes
      smb encrypt = auto
      load printers = no
      printcap name = /dev/null
      guest account = nobody
      map to guest = bad user
      hosts allow = 10.9.8.33 10.9.4.3 192.168.1.83 10.9.6.23 10.9.5.1 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
    '';
    # log level = 3

    # Don't forget to run `smbpasswd -a <user>` to set the passwords (the user must already exit)
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
        "read only" = "yes";
        writeable = "no";
      };
      music-other = {
        path = "/mnt/tank2/media/music/other";
        browseable = "yes";
        "force user" = "roon";
        "guest ok" = "no";
        "read only" = "yes";
        writeable = "no";
      };
      audiobooks = {
        path = "/mnt/tank2/media/audiobooks";
        browseable = "yes";
        "force user" = "roon";
        "guest ok" = "no";
        "read only" = "yes";
        writeable = "no";
      };
      paperless = {
        path = "/mnt/tank2/services/paperless.k8s.***REMOVED***";
        browseable = "yes";
        "force user" = "k8s-nfs";
        "guest ok" = "no";
        public = "no";
        "read only" = "no";
        writeable = "yes";
      };
    };
  };

  environment.persistence = { "/persist" = { directories = [ "/var/lib/samba" ]; }; };
}
