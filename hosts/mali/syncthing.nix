{
  config,
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
    guiAddress = "0.0.0.0:8394";
    overrideDevices = true;
    overrideFolders = true;
    key = config.sops.secrets.syncthing-key.path;
    cert = config.sops.secrets.syncthing-cert.path;
    settings = {
      gui = {
        user = "ramblurr";
        password = config.repo.secrets.local.syncthingPassword;
      };
      options = {
        urAccepted = 1; # allow anonymous usage data report
        globalAnnounceEnabled = false; # only sync locally or over vpn
      };
      devices = {
        "ipad" = {
          id = "MV4BQ23-XDBDIG6-WHBCLSE-XYRFJD7-SS7HCJP-Y7CA6EE-USJLP3Z-JDKCGAS";
        };
        "phone" = {
          id = "IZFM24Q-VTKFTBG-57BIZ4G-TJPWO2B-XW6Q2CA-S5SE6Z6-PAZMGKK-TSISTA2";
        };
        "phone2" = {
          id = "I5V5S76-7X343XH-O6DS27F-XQ6NW27-LQ65D2P-CZQRTE2-4FUI37W-CQYPOQG";
        };
        "witt" = {
          id = "TPXTYXZ-UJEBYRU-MJQN4F5-JHM6BVP-EGLQHIL-QMN7TPG-GAYMDBA-SFP26AG";
        };
        "aquinas" = {
          id = "37NYWVB-26URNF2-KFOZYXT-H4SNZ6E-OMAUOAU-NIV6RRQ-YRAPTH6-LY4HKQW";
        };
        "quine" = {
          id = "RK7O6ZN-OSUW3SM-TP2E2YZ-RBFGWK6-V2MHYEY-Z4HGCJU-EQTC4TO-72WI2QA";
        };
        "v-phone" = {
          id = "HBV2LWT-BIPOYKR-JR6SV27-TZI7PLQ-VK2UIJU-RXVCKFG-LMAFPUR-5ZDX7A2";
        };
        "v-pc" = {
          id = "EP7AND7-PUEGDL4-RUDIOJT-RYXK4LI-OHKFQAM-2VJKZ65-OGTEACX-QDVNQQD";
        };
        "pinenote" = {
          id = "3325NK7-Y2FQDAO-CAZTGTF-KLM6R4Y-GO4Y3CF-L3JUQGP-HESNL5B-NXRBGA5";
        };
        "kobo" = {
          id = "D4JC2XS-YQ75YEK-FSG6IDR-SFZBP7Z-IE74F7I-QP3EJ4T-JNII27Y-WOA67A4";
        };
        "mudita-kompakt" = {
          id = "76XIVGS-O7E2NW5-LDVRNHL-T2OIUG5-EO23U65-VNGQQUY-HZF6PCI-LMDSVAE";
        };
      };
      folders = {
        "koreader-pinenote" = {
          id = "kpphm-29veh";
          path = "/mnt/tank2/backups/koreader/pinenote";
          devices = [
            "pinenote"
            "quine"
          ];
        };
        "koreader-mudita" = {
          id = "fiqtt-jejf4";
          path = "/mnt/tank2/backups/koreader/mudita-kompakt";
          devices = [
            "mudita-kompakt"
            "quine"
          ];
        };
        "koreader-kobo" = {
          id = "xmja2-xhtrr";
          path = "/mnt/tank2/backups/koreader/kobo";
          devices = [
            "kobo"
            "quine"
          ];
        };
        "koreader-pixel" = {
          id = "gsvac-myqnf";
          path = "/mnt/tank2/backups/koreader/pixel";
          devices = [
            "phone2"
            "quine"
          ];
        };
        "Pixel9Backup" = {
          id = "dfkjb-d9yfl";
          path = "/mnt/tank2/backups/devices/pixel-9";
          devices = [
            "phone2"
            "quine"
          ];
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
