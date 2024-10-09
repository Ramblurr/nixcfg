{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
{
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 8384 ];

  sops.secrets.syncthing-key = {
    owner = "ramblurr";
  };
  sops.secrets.syncthing-cert = {
    owner = "ramblurr";
  };
  services.syncthing = {
    enable = true;
    systemService = true;
    user = "ramblurr";
    configDir = "/home/ramblurr/.config/syncthing";
    dataDir = "/home/ramblurr/.config/syncthing";
    openDefaultPorts = true;
    guiAddress = "127.0.0.1:8384";
    overrideDevices = true;
    overrideFolders = true;
    key = config.sops.secrets.syncthing-key.path;
    cert = config.sops.secrets.syncthing-cert.path;
    settings = {
      gui.theme = "black";
      options = {
        urAccepted = 1; # allow anonymous usage data report
        globalAnnounceEnabled = false; # only sync locally or over vpn
      };
      devices = {
        "ipad" = {
          id = "MV4BQ23-XDBDIG6-WHBCLSE-XYRFJD7-SS7HCJP-Y7CA6EE-USJLP3Z-JDKCGAS";
          addresses = [
            "tcp://10.9.6.15:22000"
            "tcp://100.108.66.109:22000"
          ];
        };
        "phone" = {
          id = "IZFM24Q-VTKFTBG-57BIZ4G-TJPWO2B-XW6Q2CA-S5SE6Z6-PAZMGKK-TSISTA2";
          addresses = [
            "tcp://10.9.6.14:22000"
            "tcp://100.78.82.102:22000"
          ];
        };
        "phone2" = {
          id = "I5V5S76-7X343XH-O6DS27F-XQ6NW27-LQ65D2P-CZQRTE2-4FUI37W-CQYPOQG";
          addresses = [
            "tcp://10.9.6.17:22000"
            "tcp://100.98.61.68:22000"
          ];
        };
        "mali" = {
          id = "FUWM2VN-32WHX4C-AFGU6HX-TZAITRE-PO4YKQI-UR6Z54O-DSQORX2-FJPG3AE";
          addresses = [
            "tcp://10.9.8.3:22000"
            "tcp://100.114.104.114:22000"
          ];
        };
        "quine" = {
          id = "RK7O6ZN-OSUW3SM-TP2E2YZ-RBFGWK6-V2MHYEY-Z4HGCJU-EQTC4TO-72WI2QA";
          addresses = [
            "tcp://10.9.6.138:22000"
            "tcp://100.93.18.79:22000"
          ];
        };
        "v-phone" = {
          id = "HBV2LWT-BIPOYKR-JR6SV27-TZI7PLQ-VK2UIJU-RXVCKFG-LMAFPUR-5ZDX7A2";
        };
        "v-pc" = {
          id = "EP7AND7-PUEGDL4-RUDIOJT-RYXK4LI-OHKFQAM-2VJKZ65-OGTEACX-QDVNQQD";
        };
      };
      folders = {
        "logseq" = {
          id = "lzv9p-tt1gf";
          path = "/home/ramblurr/docs/brain";
          devices = [
            "quine"
            "phone"
            "phone2"
            "ipad"
          ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "15768000"; # 180 days
            };
          };
        };
        "SNO-Noten" = {
          id = "jfi2q-111c3";
          path = "/home/ramblurr/docs/SNO/Noten - Scores";
          devices = [
            "phone2"
            "ipad"
            "quine"
          ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "1209600"; # 14 days
            };
          };
        };
        "MuseScore4" = {
          id = "nfnyf-araxw";
          path = "/home/ramblurr/docs/MuseScore4";
          devices = [ "quine" ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "1209600"; # 14 days
            };
          };
        };
        "src" = {
          id = "3afjx-a2sse";
          path = "/home/ramblurr/src";
          devices = [ "quine" ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "1209600"; # 14 days
            };
          };
        };
        "work-src" = {
          id = "pcdsa-wgsys";
          path = "/home/ramblurr/work";
          devices = [ "quine" ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "1209600"; # 14 days
            };
          };
        };
        "inbox" = {
          id = "n0zj4-mpb2o";
          path = "/home/ramblurr/sync/inbox";
          devices = [
            "quine"
            "phone"
            "phone2"
            "ipad"
          ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "15768000"; # 180 days
            };
          };
        };
        "viki" = {
          id = "emomw-9nc2j";
          path = "/home/ramblurr/sync/viki";
          devices = [
            "quine"
            "phone"
            "phone2"
            "ipad"
            "v-phone"
            "v-pc"
          ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "15768000"; # 180 days
            };
          };
        };
      };
    };
  };
  # FIX: home-manager impermanence
  # when using with home-manager impermanence we need to ensure that home-manager activates before
  # syncthing. otherwise the syncthing init will create ~/.config/syncthing, but ~/.config will be created
  # with root:root ownership.
  systemd.services.syncthing.after = [ "home-manager-ramblurr.service" ];
  systemd.services.syncthing-init.after = [ "home-manager-ramblurr.service" ];
  # END FIX: home-manager impermanence
}
