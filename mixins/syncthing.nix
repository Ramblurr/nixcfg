{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  config = {
    networking.firewall.interfaces."tailscale0".allowedTCPPorts = [8384];

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
      devices = {
        "rorty" = {id = "OURFCGA-AT7K6SV-KBT6DDR-UJIGS5A-SYR3SWR-BGIJQ3I-IMP7VCA-MKSGVAB";};
        "ipad" = {id = "MV4BQ23-XDBDIG6-WHBCLSE-XYRFJD7-SS7HCJP-Y7CA6EE-USJLP3Z-JDKCGAS";};
        "phone" = {id = "IZFM24Q-VTKFTBG-57BIZ4G-TJPWO2B-XW6Q2CA-S5SE6Z6-PAZMGKK-TSISTA2";};
        "mali" = {id =  "SS4DIYV-GL4SUAO-3H6JWGX-Q2PBKNT-UWII65N-ZNTEQBU-N6D7XFQ-V2K7TQU";};
        "aquinas" = {id = "B4Z7XOT-UQIP4TE-SY6AHMM-KWG6DZB-V254DBN-65FU4D2-4W5V5IL-XY7K5AM";};
        "v-phone" = {id = "HBV2LWT-BIPOYKR-JR6SV27-TZI7PLQ-VK2UIJU-RXVCKFG-LMAFPUR-5ZDX7A2";};
        "v-pc" = {id = "EP7AND7-PUEGDL4-RUDIOJT-RYXK4LI-OHKFQAM-2VJKZ65-OGTEACX-QDVNQQD";};
      };
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8384";
      overrideDevices = true;
      overrideFolders = true;
      key = config.sops.secrets.syncthing-key.path;
      cert = config.sops.secrets.syncthing-cert.path;
      extraOptions = {
        gui.theme = "black";
      };
      folders = {
        "logseq" = {
          id = "lzv9p-tt1gf";
          path = "/home/ramblurr/docs/brain";
          devices = ["aquinas" "rorty" "phone" "ipad"];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour
              maxAge = "15768000"; # 180 days
            };
          };
        };
        "src" = {
          id = "3afjx-a2sse";
          path = "/home/ramblurr/src";
          devices = ["aquinas" "rorty"];
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
          devices = ["aquinas" "rorty"];
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
          devices = ["aquinas" "rorty" "phone" "ipad"];
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
          devices = ["aquinas" "rorty" "phone" "ipad" "v-phone" "v-pc"];
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
    # FIX: home-manager impermanence
    # when using with home-manager impermanence we need to ensure that home-manager activates before
    # syncthing. otherwise the syncthing init will create ~/.config/syncthing, but ~/.config will be created
    # with root:root ownership.
    systemd.services.syncthing.after = ["home-manager-ramblurr.service"];
    systemd.services.syncthing-init.after = ["home-manager-ramblurr.service"];
    # END FIX: home-manager impermanence
  };
}
