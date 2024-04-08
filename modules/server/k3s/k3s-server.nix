{
  options,
  config,
  lib,
  pkgs,
  inputs,
  unstable,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.server.k3s-server;
  hostName = config.networking.hostName;
  domain = config.networking.domain;
  kubeVipDaemonSet = pkgs.writeTextFile {
    name = "kube-vip-ds.yaml";
    text = builtins.readFile (
      pkgs.substituteAll {
        src = ./kube-vip-ds.yaml;
        endpointVip = cfg.endpointVip;
      }
    );
  };
  kubeVipRBAC = pkgs.writeTextFile {
    name = "kube-vip-rbac.yaml";
    text = builtins.readFile ./kube-vip-rbac.yaml;
  };

  ciliumInitHelmchart = pkgs.writeTextFile {
    name = "custom-cilium-helmchart.yaml";
    text = builtins.readFile (
      pkgs.substituteAll {
        src = ./custom-cilium-helmchart.yaml;
        clusterCIDR = cfg.clusterCIDR;
        clusterName = cfg.clusterName;
        ciliumDevices = builtins.toJSON (cfg.ciliumDevices);
      }
    );
  };
  ciliumPythonEnv = pkgs.python311.withPackages (python-pkgs: [ python-pkgs.kubernetes ]);
  ciliumInstallScript = pkgs.writeTextFile {
    name = "cilium-install.py";
    text = builtins.readFile (
      pkgs.substituteAll {
        src = ./cilium-install.py;
        kubeconfig = "/etc/rancher/k3s/k3s.yaml";
        nodename = hostName;
      }
    );
  };
in
{
  options.modules.server.k3s-server = {
    enable = mkBoolOpt false;
    started = mkBoolOpt true;
    bootstrapEnable = mkBoolOpt false;
    bootstrapAddr = mkStrOpt "";
    ciliumBootstrap.enable = mkBoolOpt false;
    tokenFile = mkStrOpt "/etc/k3s-token";
    clusterCIDR = mkStrOpt "10.69.0.0/16";
    serviceCIDR = mkStrOpt "10.96.0.0/16";
    clusterName = mkStrOpt "home-kubernetes";
    endpointVip = mkStrOpt "";
    nodeIp = mkStrOpt "";
    ciliumDevices = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };
  config =
    let
      disabledServices = [
        "flannel"
        "local-storage"
        "metrics-server"
        "servicelb"
        "traefik"
      ];
      extraFlags = [
        # We will use cilium instead of flannel
        "--flannel-backend=none"
        #"--node-taint \"node-role.kubernetes.io/control-plane=true:NoSchedule\""
        # Prevent the node from being upgraded by k3s (we use NixOS instead)
        ''--node-label "k3s-upgrade=false"''
        "--disable-cloud-controller"
        "--disable-kube-proxy"
        "--embedded-registry"
        "--etcd-expose-metrics"
        "--disable-network-policy"
        "--kube-apiserver-arg anonymous-auth=true"
        "--kube-controller-manager-arg bind-address=0.0.0.0"
        "--kube-scheduler-arg bind-address=0.0.0.0"
        "--kubelet-arg image-gc-low-threshold=50"
        "--kubelet-arg image-gc-high-threshold=55"
        "--node-ip ${cfg.nodeIp}"
        "--pause-image registry.k8s.io/pause:3.9"
        "--secrets-encryption"
        "--tls-san ${cfg.endpointVip}"
        "--write-kubeconfig-mode 0644"
        # Specify IP address allocation range for pods
        "--cluster-cidr ${cfg.clusterCIDR}"
        "--service-cidr ${cfg.serviceCIDR}"
        #"--container-runtime-endpoint unix:///run/containerd/containerd.sock"
      ];
      disableFlags = map (service: "--disable ${service}") disabledServices;
      combinedFlags = concatLists [
        disableFlags
        extraFlags
      ];
    in
    mkIf cfg.enable {
      modules.server.k3s-common.enable = true;
      services.k3s = {
        enable = cfg.started;
        role = "server";
        serverAddr = cfg.bootstrapAddr;
        clusterInit = cfg.bootstrapEnable;
        tokenFile = cfg.tokenFile;
        extraFlags = concatStringsSep " " combinedFlags;
      };

      environment.systemPackages = with pkgs; [
        unstable.kubectl
        ciliumPythonEnv
      ];

      networking.firewall.allowedTCPPorts = [
        6443 # KubeApi (TCP)
        2379 # etcd clients
        2380 # etcd peers
      ];

      systemd.tmpfiles.rules =
        [
          "d /var/lib/rancher/k3s/server/manifests 0775 root root -"
          "d /etc/rancher/custom 0775 root root -"
          "L+ /var/lib/rancher/k3s/server/manifests/kube-vip-0-rbac.yaml - - - - ${kubeVipRBAC}"
          "L+ /var/lib/rancher/k3s/server/manifests/kube-vip-1-ds.yaml - - - - ${kubeVipDaemonSet}"
        ]
        ++ (
          if cfg.ciliumBootstrap.enable then
            [ "L /etc/rancher/custom/custom-cilium-helmchart.yaml - - - - ${ciliumInitHelmchart}" ]
          else
            [ ]
        );

      systemd.services.cilium-bootstrap = mkIf cfg.ciliumBootstrap.enable {
        after = [ "k3s.service" ];
        description = "Bootstrap Cilium";
        requires = [ "k3s.service" ];
        serviceConfig = {
          Type = "oneshot";
        };
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${ciliumPythonEnv}/bin/python3 -u ${ciliumInstallScript}";
        };
      };

      #k3s = {
      #  after = ["tailscale-up.service"];
      #  requires = ["tailscale-up.service"];
      #};
    };
}
