{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.profiles.k3s-node;
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
in {
  options.modules.profiles.k3s-node = {
    enable = mkBoolOpt false;
    hostname = mkStrOpt "";
    defaultSopsFile = mkOption { type = lib.types.path; };
    user = mkOption {
      type = types.attrs;
      default = { };
      description = "User config.";
    };
    clusterSettings = mkOption {
      type = types.attrs;
      description = "Cluster and node settings";
    };
  };
  config = let
    nodeSettings = cfg.clusterSettings.servers.${cfg.hostname};
    bootstrapNode = cfg.clusterSettings.bootstrapNode;
    bootstrapNodeAddr = cidrToIp cfg.clusterSettings.servers.${bootstrapNode}.mgmtCIDR;
    bootstrapEnable = nodeSettings.bootstrapEnable or false;
    isControlPlane = nodeSettings.isControlPlane or false;
  in mkIf cfg.enable {
    ###########
    ## Basic ##
    ###########
    sops.defaultSopsFile = cfg.defaultSopsFile;
    time.timeZone = "Europe/Berlin";
    i18n.defaultLocale = "en_US.utf8";
    sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
    documentation.nixos.enable = false;
    documentation.doc.enable = false;
    # Modules and sysctl settings common to k3s nodes
    boot.kernelModules = [ "br_netfilter" "ceph" "ip_vs" "ip_vs_rr" "nbd" "overlay" "rbd" ];
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
      services = { sshd.enable = true; };
      editors = { vim.enable = true; };
      impermanence.enable = true;
      boot.zfs = {
        enable = true;
        encrypted = true;
        rootPool = "rpool";
        scrubPools = [ "rpool" ];
        autoSnapshot.enable = false;
      };
      server = {
        smtp-external-relay.enable = false;
        k3s-server = mkIf isControlPlane {
          enable = true;
          started = true;
          ciliumBootstrap.enable = bootstrapEnable;
          ciliumDevices = [ "brmgmt9" "brdata11" "brprim4" ];
          endpointVip = cfg.clusterSettings.endpointVip;
          clusterName = cfg.clusterSettings.clusterName;
          nodeIp = cidrToIp nodeSettings.mgmtCIDR;
          bootstrapEnable = bootstrapEnable;
          bootstrapAddr = if bootstrapEnable then "" else "https://${bootstrapNodeAddr}:6443";
        };
        k3s-agent = mkIf (!isControlPlane) {
          enable = true;
          started = true;
          serverAddr = "https://${bootstrapNodeAddr}:6443";
          nodeIp = cidrToIp nodeSettings.mgmtCIDR;
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
        extraGroups = [ "libvirtd" "wheel" ];
      };
    };
    ##############
    ## Services ##
    ##############
    services.smartd.enable = true;
    services.rpcbind.enable = true;

    services.prometheus = {
      exporters = {
        node = {
          enable = false;
          enabledCollectors = [ "systemd" ];
          disabledCollectors = [ "textfile" ];
        };
        zfs = {
          enable = true;
          port = 9002;
        };
        smartctl = {
          enable = false;
          port = 9003;
        };
      };
    };

    networking.firewall.allowedTCPPorts = [
      config.services.prometheus.exporters.node.port
      config.services.prometheus.exporters.zfs.port
      config.services.prometheus.exporters.smartctl.port
      9926 # ceph-exporter
      9283 # ceph mgr exporter
      9080 # csi-rbdplugin-metrics
      9081 # csi-cephfsplugin-metrics
      9962 # cilium agent exporter
      9963 # cilium operator exporter
      9965 # cilium hubble exporter
      10257 # kube-controller-manager exporter
      2381 # kube-etcd exporter
      10259 # kube-scheduler exporter
      9633 # smartctl exporter
    ];

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
      python311

      tcpdump
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
      text = cfg.clusterSettings.k3sToken;
    };

    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [ "/var/lib/nixos" "/var/lib/systemd/coredump" ];
      files = [ ];
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

    home-manager.users."${cfg.user.username}" = { pkgs, ... }@hm: {
      persistence = { directories = [ ".config/k9s" ".config/kube" ]; };
    };

    ################
    ## Networking ##
    ################
    networking.usePredictableInterfaceNames = true;
    networking.firewall.allowPing = true;
    networking.nameservers = [ "192.168.1.3" "10.9.4.4" ];

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
        "40-${nodeSettings.mgmtIface}" = {
          matchConfig.Name = "${nodeSettings.mgmtIface}";
          vlan = [ "vlmgmt9" "vlprim4" ];
        };
        "40-${nodeSettings.dataIface}" = {
          matchConfig = { Name = "${nodeSettings.dataIface}"; };
          networkConfig = { Description = "physical 10gbe"; };
          linkConfig = { MTUBytes = "9000"; };
          vlan = [ "vldata11" ];
        };
        "45-vlprim4" = {
          matchConfig = { Name = "vlprim4"; };
          networkConfig = { Bridge = "brprim4"; };
        };
        "45-vldata11" = {
          matchConfig = { Name = "vldata11"; };
          networkConfig = { Bridge = "brdata11"; };
        };
        "45-vlmgmt9" = {
          matchConfig = { Name = "vlmgmt9"; };
          networkConfig = { Bridge = "brmgmt9"; };
        };

        "50-brprim4" = if nodeSettings.vlanPrimaryEnabled then {
          matchConfig = { Name = "brprim4"; };
          networkConfig = {
            DHCP = "no";
            Address = nodeSettings.primCIDR;
            Description = "primary VLAN";
          };
        } else {
          # My k3s nodes do not have an ip address on the primary vlan
          # however VMs and k8s workloads running on these nodes might want to expose a service
          # over this vlan, so we configure the bridge interface anyways
          matchConfig = { Name = "brprim4"; };
          networkConfig = {
            DHCP = "no";
            Description = "Bridge for primary vlan, only for VMs and k8s workloads";
          };
          # might want this in the future for cilium l2 announcements
          #routes = [
          #  {
          #    routeConfig = {
          #      Destination = "10.9.4.0/22";
          #    };
          #  }
          #];
        };
        "50-brmgmt9" = {
          matchConfig = { Name = "brmgmt9"; };
          networkConfig = {
            DHCP = "no";
            Address = nodeSettings.mgmtCIDR;
            Gateway = cfg.clusterSettings.mgmtGateway;
            Description = "mgmt VLAN";
          };
        };
        "50-brdata11" = {
          matchConfig = { Name = "brdata11"; };
          networkConfig = {
            DHCP = "no";
            Address = nodeSettings.dataCIDR;
            Description = "data 10GbE VLAN";
          };
        };
      };
    };
  };
}
