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
        urAccepted = -1; # allow anonymous usage data report
        urSeen = 3;
        globalAnnounceEnabled = false; # only sync locally or over vpn
      };
      devices = {
        "witt" = {
          id = "TPXTYXZ-UJEBYRU-MJQN4F5-JHM6BVP-EGLQHIL-QMN7TPG-GAYMDBA-SFP26AG";
          addresses = [
            "tcp://10.9.6.18:22000"
            "tcp://100.126.16.31:22000"
          ];
        };
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
        };
        "aquinas" = {
          id = "37NYWVB-26URNF2-KFOZYXT-H4SNZ6E-OMAUOAU-NIV6RRQ-YRAPTH6-LY4HKQW";
          addresses = [
            "tcp://10.9.6.90:22000"
            "tcp://100.113.239.46:22000"
          ];
        };
        "v-phone" = {
          id = "HBV2LWT-BIPOYKR-JR6SV27-TZI7PLQ-VK2UIJU-RXVCKFG-LMAFPUR-5ZDX7A2";
        };
        "v-pc" = {
          id = "EP7AND7-PUEGDL4-RUDIOJT-RYXK4LI-OHKFQAM-2VJKZ65-OGTEACX-QDVNQQD";
        };
        "pinenote" = {
          id = "3325NK7-Y2FQDAO-CAZTGTF-KLM6R4Y-GO4Y3CF-L3JUQGP-HESNL5B-NXRBGA5";
          addresses = [
            "tcp://10.9.6.36:22000"
            "tcp://100.114.233.33:22000"
          ];
        };
        "kobo" = {
          id = "D4JC2XS-YQ75YEK-FSG6IDR-SFZBP7Z-IE74F7I-QP3EJ4T-JNII27Y-WOA67A4";
          addresses = [
            "tcp://10.9.6.96:22000"
            "tcp://100.87.226.94:22000"
          ];
        };
        "mudita-kompact" = {
          id = "76XIVGS-O7E2NW5-LDVRNHL-T2OIUG5-EO23U65-VNGQQUY-HZF6PCI-LMDSVAE";
          addresses = [
            "tcp://10.9.6.37:22000"
            "tcp://100.91.180.98:22000"
          ];
        };
      };
      folders = {
        "Pixel9Backup" = {
          id = "dfkjb-d9yfl";
          path = "/home/ramblurr/docs/backups/pixel-9";
          devices = [
            "phone2"
          ];
        };
        "S21Backup" = {
          id = "kbhu0-k5zxq";
          path = "/home/ramblurr/docs/backups/samsung-s21";
          devices = [
            "phone"
          ];
        };
        "logseq" = {
          id = "lzv9p-tt1gf";
          path = "/home/ramblurr/docs/brain";
          devices = [
            "aquinas"
            "witt"
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
            "aquinas"
            "phone2"
            "ipad"
            "witt"
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
          devices = [
            "aquinas"
            "phone2"
            "ipad"
            "witt"
          ];
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
          devices = [
            "aquinas"
            "witt"
          ];
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
          devices = [
            "aquinas"
            "witt"
          ];
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
            "aquinas"
            "witt"
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
            "aquinas"
            "witt"
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
