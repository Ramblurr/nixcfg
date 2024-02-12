{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.profiles.k3s-node;
in {
  options.modules.profiles.k3s-node = {
    enable = mkBoolOpt false;
    hostname = mkStrOpt "";
    defaultSopsFile = mkOption {
      type = lib.types.path;
    };
    user = mkOption {
      type = types.attrs;
      default = {};
      description = "User config.";
    };
    server.enable = mkBoolOpt false;
    agent.enable = mkBoolOpt false;
    clusterSettings = mkOption {
      type = types.attrs;
      default = {
        k3s-token = mkStrOpt "";
      };
    };
  };
  config = let
    nodeSettings = cfg.clusterSettings.servers.${cfg.hostname};
  in
    mkIf cfg.enable {
      ###########
      ## Basic ##
      ###########
      sops.defaultSopsFile = cfg.defaultSopsFile;
      time.timeZone = "Europe/Berlin";
      i18n.defaultLocale = "en_US.utf8";
      sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
      documentation.nixos.enable = false;
      documentation.doc.enable = false;

      # Modules and sysctl settings common to k3s nodes
      boot.kernelModules = ["br_netfilter" "ceph" "ip_vs" "ip_vs_rr" "nbd" "overlay" "rbd"];
      boot.kernel.sysctl = {
        "fs.inotify.max_queued_events" = 65536;
        "fs.inotify.max_user_watches" = 524288;
        "fs.inotify.max_user_instances" = 8192;
      };

      #######################
      ## My Custom Modules ##
      #######################
      modules = {
        shell = {
          htop.enable = true;
          tmux.enable = true;
          zsh.enable = true;
        };
        services = {
          sshd.enable = true;
        };
        editors = {
          vim.enable = true;
        };
        impermanence.enable = true;
        boot.zfs = {
          enable = true;
          encrypted = true;
          rootPool = "rpool";
          scrubPools = ["rpool"];
          autoSnapshot.enable = false;
        };
        server = {
          smtp-external-relay.enable = false;
          k3s-server = mkIf cfg.server.enable {
            enable = true;
            cilium.enable = true;
            endpointVip = cfg.clusterSettings.endpointVip;
            clusterName = cfg.clusterSettings.clusterName;
            nodeIp = nodeSettings.nodeIp;
            bootstrapEnable = nodeSettings.bootstrapEnable;
          };
        };
        # vpn.tailscale.enable = true;
        firewall.enable = true;
        security.default.enable = true;
        networking.default.enable = true;
        networking.default.hostName = cfg.hostname;
        users.enable = true;
        users.primaryUser = {
          username = cfg.user.username;
          name = cfg.user.name;
          homeDirectory = cfg.user.homeDirectory;
          signingKey = cfg.user.signingKey;
          email = cfg.user.email;
          passwordSecretKey = cfg.user.passwordSecretKey;
          defaultSopsFile = cfg.defaultSopsFile;
          shell = pkgs.zsh;
          extraGroups = [
            "libvirtd"
            "wheel"
          ];
        };
      };
      ##############
      ## Services ##
      ##############
      services.smartd.enable = true;

      ##############
      ## Packages ##
      ##############

      environment.systemPackages = with pkgs; [
        smartmontools
        ncdu
        lshw
        vifm
        yq-go
        jq

        fluxcd
        kubectl
        cilium-cli
        k9s
        hubble
      ];

      #########################
      ## Files & Persistence ##
      #########################

      environment.variables.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
      environment.etc."k3s-token" = {
        user = "root";
        mode = "0600";
        text = cfg.clusterSettings.k3s-token;
      };

      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
        ];
        files = [
        ];
      };

      # Ensures that /persist/home/ramblurr exists with the correct ownership and perms before the home-manager service runs
      systemd.tmpfiles.rules = [
        "d /persist/home/${cfg.user.username} 700 ${cfg.user.username} ${cfg.user.username}"
        "d /persist/home/${cfg.user.username}/.config 0775 ${cfg.user.username} ${cfg.user.username}  -"
        "d /persist/home/${cfg.user.username}/.local 755 ${cfg.user.username} ${cfg.user.username}"
        "d /persist/home/${cfg.user.username}/.local/state 755 ${cfg.user.username} ${cfg.user.username}"
        "d /persist/home/${cfg.user.username}/.local/state/zsh 755 ${cfg.user.username} ${cfg.user.username}"
        "d /persist/home/${cfg.user.username}/.config/kube 0770 ${cfg.user.username} ${cfg.user.username}  -"
        "d /persist/home/${cfg.user.username}/.config/k9s 0770 ${cfg.user.username} ${cfg.user.username}  -"
        "L+ /persist/home/${cfg.user.username}/.config/kube/config  - - - - /etc/rancher/k3s/k3s.yaml"
      ];

      home-manager.users."${cfg.user.username}" = {pkgs, ...} @ hm: {
        persistence = {
          directories = [
            ".config/k9s"
            ".config/kube"
          ];
        };
      };

      ################
      ## Networking ##
      ################
      networking.usePredictableInterfaceNames = true;
      networking.firewall.allowPing = true;
      networking.nameservers = ["192.168.1.3" "10.9.4.4"];

      # Useful if you need to troubleshoot systemd-networkd
      # systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];

      systemd.network = {
        netdevs = {
          "20-vlprim4" = {
            netdevConfig = {
              Kind = "vlan";
              Name = "vlprim4";
            };
            vlanConfig.Id = 4;
          };
          "20-vlmgmt9" = {
            netdevConfig = {
              Kind = "vlan";
              Name = "vlmgmt9";
            };
            vlanConfig.Id = 9;
          };
          "20-vldata11" = {
            netdevConfig = {
              Kind = "vlan";
              Name = "vldata11";
              MTUBytes = "9000";
            };
            vlanConfig.Id = 11;
          };
          "30-brprim4" = {
            netdevConfig = {
              Name = "brprim4";
              Kind = "bridge";
            };
          };
          "30-brmgmt9" = {
            netdevConfig = {
              Name = "brmgmt9";
              Kind = "bridge";
            };
          };
          "30-brdata11" = {
            netdevConfig = {
              Name = "brdata11";
              Kind = "bridge";
              MTUBytes = "9000";
            };
          };
        };

        networks = {
          "40-eno1" = {
            matchConfig.Name = "eno1";
            vlan = [
              "vlmgmt9"
              "vlprim4"
            ];
          };
          "40-enp6s0" = {
            matchConfig = {Name = "enp6s0";};
            networkConfig = {Description = "10gbe";};
            linkConfig = {MTUBytes = "9000";};
            vlan = [
              "vldata11"
            ];
          };
          "45-vlprim4" = {
            matchConfig = {Name = "vlprim4";};
            networkConfig = {
              Bridge = "brprim4";
            };
          };
          "45-vldata11" = {
            matchConfig = {Name = "vldata11";};
            networkConfig = {
              Bridge = "brdata11";
            };
          };
          "45-vlmgmt9" = {
            matchConfig = {Name = "vlmgmt9";};
            networkConfig = {
              Bridge = "brmgmt9";
            };
          };

          "50-brprim4" = {
            matchConfig = {Name = "brprim4";};
            networkConfig = {
              DHCP = "no";
              Description = "Bridge for primary vlan, only for VMs.";
            };
          };
          "50-brmgmt9" = {
            matchConfig = {Name = "brmgmt9";};
            networkConfig = {
              DHCP = "no";
              Address = nodeSettings.mgmtAddress;
              Gateway = cfg.clusterSettings.mgmtGateway;
              Description = "mgmt VLAN";
            };
          };
          "50-brdata11" = {
            matchConfig = {Name = "brdata11";};
            networkConfig = {
              DHCP = "no";
              Address = nodeSettings.dataAddress;
              Description = "data 10GbE VLAN";
            };
          };
        };
      };
    };
}
