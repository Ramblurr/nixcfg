{ config, ... }:
let
  user = config.modules.users.primaryUser.username;
  home = config.modules.users.primaryUser.homeDirectory;
in
{
  sops.secrets.syncthing-key.owner = user;
  sops.secrets.syncthing-cert.owner = user;

  services.syncthing = {
    enable = true;
    systemService = true;
    inherit user;
    configDir = "${home}/.config/syncthing";
    dataDir = "${home}/.config/syncthing";
    guiAddress = "127.0.0.1:8384";
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;
    key = config.sops.secrets.syncthing-key.path;
    cert = config.sops.secrets.syncthing-cert.path;
    settings = {
      devices = {
        "mali" = {
          id = "FUWM2VN-32WHX4C-AFGU6HX-TZAITRE-PO4YKQI-UR6Z54O-DSQORX2-FJPG3AE";
        };
        "Pixel 6a" = {
          id = "HBV2LWT-BIPOYKR-JR6SV27-TZI7PLQ-VK2UIJU-RXVCKFG-LMAFPUR-5ZDX7A2";
        };
        "SM-G998B" = {
          id = "IZFM24Q-VTKFTBG-57BIZ4G-TJPWO2B-XW6Q2CA-S5SE6Z6-PAZMGKK-TSISTA2";
        };
        "Casey Pixel 9" = {
          id = "I5V5S76-7X343XH-O6DS27F-XQ6NW27-LQ65D2P-CZQRTE2-4FUI37W-CQYPOQG";
        };
        "Casey's iPad" = {
          id = "MV4BQ23-XDBDIG6-WHBCLSE-XYRFJD7-SS7HCJP-Y7CA6EE-USJLP3Z-JDKCGAS";
        };
        "quine" = {
          id = "RK7O6ZN-OSUW3SM-TP2E2YZ-RBFGWK6-V2MHYEY-Z4HGCJU-EQTC4TO-72WI2QA";
        };
        "witt" = {
          id = "TPXTYXZ-UJEBYRU-MJQN4F5-JHM6BVP-EGLQHIL-QMN7TPG-GAYMDBA-SFP26AG";
        };
        "aquinas" = {
          id = "37NYWVB-26URNF2-KFOZYXT-H4SNZ6E-OMAUOAU-NIV6RRQ-YRAPTH6-LY4HKQW";
        };
      };

      folders = {
        "default" = {
          id = "default";
          label = "Default Folder";
          path = "${home}/Sync/Default Folder";
          devices = [ ];
          paused = true;
        };
        "CaseyPC" = {
          id = "emomw-9nc2j";
          path = "${home}/Sync/CaseyPC";
          devices = [
            "Pixel 6a"
            "SM-G998B"
            "Casey Pixel 9"
            "Casey's iPad"
            "quine"
            "witt"
            "aquinas"
          ];
          paused = true;
          versioning = {
            type = "staggered";
            params.maxAge = "7776000";
          };
        };
        "My Devices" = {
          id = "rspvm-jqedw";
          path = "${home}/Sync/My Devices";
          devices = [ ];
          paused = true;
        };
      };
    };
  };

  systemd.services.syncthing = {
    after = [ "home-manager-${user}.service" ];
    environment.STNODEFAULTFOLDER = "true";
  };
  systemd.services.syncthing-init.after = [ "home-manager-${user}.service" ];
}
