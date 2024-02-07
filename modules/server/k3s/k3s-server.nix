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
  cfg = config.modules.server.k3s-server;
  k3s-package = pkgs.unstable.k3s_1_29;
  hostName = config.networking.hostName;
  domain = config.networking.domain;
in {
  options.modules.server.k3s-server = {
    enable = mkBoolOpt false;
    tokenFile = mkStrOpt "/etc/k3s-token";
    clusterCidr = mkStrOpt "10.69.0.0/16";
    serviceCidr = mkStrOpt "10.96.0.0/16";
    endpointVip = mkStrOpt "";
    nodeIp = mkStrOpt "";
  };
  config = let
    disabledServices = ["coredns" "flannel" "local-storage" "metrics-server" "servicelb" "traefik"];
    extraFlags = [
      "--flannel-backend=none"
      "--node-taint \"node-role.kubernetes.io/control-plane=true:NoSchedule\""
      "--node-label \"k3s-upgrade=false\""
      "--disable-cloud-controller"
      "--disable-kube-proxy"
      "--embedded-registry"
      "--etcd-expose-metrics"
      "--disable-network-policy"
      "--kube-apiserver-arg anonymous-auth=true"
      "--kube-controller-manager-arg bind-address=0.0.0.0"
      "--kube-scheduler-arg bind-address=0.0.0.0"
      "--kubelet-arg image-gc-low-threshold=50,image-gc-high-threshold=55"
      "--node-ip ${cfg.nodeIp}"
      "--pause-image registry.k8s.io/pause:3.9"
      "--secrets-encryption"
      "--service-cidr 10.33.0.0/16"
      "--tls-san ${cfg.endpointVip}"
      "--write-kubeconfig-mode 0644"
      "--cluster-cidr ${cfg.clusterCidr}"
      "--service-cidr ${cfg.serviceCidr}"
      "--container-runtime-endpoint unix:///run/containerd/containerd.sock"
    ];
    disableFlags = map (service: "--disable ${service}") disabledServices;
    combinedFlags = concatLists [disableFlags extraFlags];
  in
    mkIf cfg.enable {
      modules.agent.k3s-common.enable = true;
      services.k3s = {
        enable = true;
        package = k3s-package;
        role = "server";
        tokenFile = cfg.tokenFile;
        extraFlags = concatStringsSep " " combinedFlags;
      };

      environment.systemPackages = with pkgs; [k3s-package unstable.kubectl];
      networking.firewall.allowedTCPPorts = [
        6443 # KubeApi (TCP)
      ];
    };
}
