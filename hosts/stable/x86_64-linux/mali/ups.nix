{
  config,
  lib,
  pkgs,
  ...
}:
{
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  power.ups = {
    enable = true;
    ups."apc" = {
      driver = "usbhid-ups";
      port = "auto";
      description = "APC UPS";
      directives = [ "default.battery.charge.low = 50" ];
    };
    users = {
      upsmon = {
        passwordFile = config.sops.secrets.upsAdminPassword.path;
        upsmon = "master";
      };
    };
    upsmon = {
      monitor."apc" = {
        system = "ups@localhost:3493";
        user = "upsmon";
        passwordFile = config.sops.secrets.upsAdminPassword.path;
        powerValue = 1;
      };
      settings = {
        MINSUPPLIES = 1;
        RUN_AS_USER = "root";

        SHUTDOWNCMD = "shutdown -h 0";
        POLLFREQ = 5;
        POLLFREQALERT = 5;
        HOSTSYNC = 15;
        DEADTIME = 15;
        RBWARNTIME = 43200;
        NOCOMMWARNTIME = 300;
        FINALDELAY = 5;

        NOTIFYCMD = "/run/current-system/sw/bin/upssched";

        NOTIFYFLAG = [
          [
            "ONLINE"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "ONBATT"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "LOWBATT"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "FSD"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "COMMOK"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "COMMBAD"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "SHUTDOWN"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "REPLBATT"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "NOCOMM"
            "SYSLOG+WALL+EXEC"
          ]
          [
            "NOPARENT"
            "SYSLOG+WALL+EXEC"
          ]
        ];
      };
    };
  };

  #sops.secrets.upsd-users = {
  #  format = "binary";
  #  sopsFile = ./upsd.sops.users;
  #};
  #sops.secrets.upsmon-conf = {
  #  format = "binary";
  #  sopsFile = ./upsmon.sops.conf;
  #};
  sops.secrets.upsAdminPassword = {
    mode = "0440";
    group = "nut";
  };
  #environment.etc."nut/upsd.users".source = config.sops.secrets.upsd-users.path;
  #environment.etc."nut/upsmon.conf".source = config.sops.secrets.upsmon-conf.path;
  #environment.etc."nut/upsd.conf".text = ''
  #  LISTEN 127.0.0.1 3493
  #'';
  users.users.nut = {
    #uid = 84;
    isSystemUser = true;
    home = "/var/lib/nut";
    createHome = true;
    group = "nut";
    description = "UPS monitor user";
  };
  users.groups."nut" = {
    #gid = 84;
  };
  systemd.services.upsd.serviceConfig = {
    User = "root";
    Group = "nut";
  };

  systemd.services.upsdrv.serviceConfig = {
    User = "root";
    Group = "nut";
  };

  systemd.services.upsdrv.serviceConfig = {
    Restart = lib.mkOverride 0 "on-failure";
    RestartSec = "3";
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [ "/var/lib/nut" ];
  };
}
