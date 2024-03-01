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
with lib.my; let
  cfg = config.modules.server.k3s-common;
  withImpermanence = config.modules.impermanence.enable;
  k3s-package = unstable.k3s_1_29;
in {
  options.modules.server.k3s-common = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.k3s.package = k3s-package;
    # Based on https://github.com/TUM-DSE/doctor-cluster-config/tree/master/modules/k3s
    virtualisation.containerd.enable = true;
    #virtualisation.containerd.settings = {
    #  version = 2;
    #  plugins."io.containerd.grpc.v1.cri" = {
    #    cni.conf_dir = "/var/lib/rancher/k3s/agent/etc/cni/net.d/";
    #    # FIXME: upstream
    #    cni.bin_dir = "${pkgs.runCommand "cni-bin-dir" {} ''
    #      mkdir -p $out
    #      ln -sf ${pkgs.cni-plugins}/bin/* ${pkgs.cni-plugin-flannel}/bin/* $out
    #    ''}";
    #  };
    #};

    environment.systemPackages = [
      k3s-package
      pkgs.nfs-utils
      pkgs.cryptsetup
      pkgs.gptfdisk
      (pkgs.writeShellScriptBin "k3s-reset-node" (builtins.readFile ./k3s-reset-node))
    ];
    systemd.services.k3s = {
      wants = ["containerd.service"];
      after = ["containerd.service"];
    };

    systemd.services.containerd.serviceConfig = {
      ExecStartPre = [
        "-${pkgs.zfs}/bin/zfs create -o mountpoint=/var/lib/containerd/io.containerd.snapshotter.v1.zfs rpool/encrypted/containerd"
      ];
    };

    networking.firewall.allowedTCPPortRanges = [
      {
        from = 2379;
        to = 2380;
      } # cilium etcd access

      {
        from = 6800;
        to = 7300;
      } # Ceph OSDs
    ];
    networking.firewall.allowedUDPPortRanges = [
      {
        from = 33434;
        to = 34000;
      } # traceroute
    ];
    networking.firewall.allowedTCPPorts = [
      80 # Ceph RGW
      3300 # Ceph monitor
      4240 # cilium healthcheck
      4244 # cilium hubble server
      4245 # cilium hubble relay
      4250 # cilium mutual auth
      4251 # cilium spire agent healthcheck
      5001 # k3s embedded registry p2p
      6443 # k3s embedded registry local OCI registry
      6789 # Ceph monitor
      7472 # MetalLB (TCP+UDP)
      7473 # MetalLB FRR (TCP+UDP)
      7946 # MetalLB (TCP+UDP)
      8080 # Ceph dashboard
      9962 # cilium-agent Prometheus metrics
      9963 # cilium-operator Prometheus metrics
      9964 # cilium-envoy Prometheus metrics
      10250 # K3s Metrics Server (TCP)
      10254 # MetalLB Metrics Export (TCP)
    ];
    networking.firewall.allowedUDPPorts = [
      7472 # MetalLB (TCP+UDP)
      7473 # MetalLB FRR (TCP+UDP)
      7946 # MetalLB (TCP+UDP)
      8472 # cilium  VXLAN
      51871 # WireGuard encryption tunnel endpoint
    ];

    environment.etc = {
      "rancher/k3s/registries.yaml" = {
        source = ./registries.yaml;
        mode = "0600";
      };
    };
    environment.persistence."/persist" = mkIf withImpermanence {
      hideMounts = true;
      directories = [
        "/var/lib/rancher"
        "/var/lib/kubelet"
        "/var/lib/containerd"
        "/etc/rancher"
        "/var/lib/cni"
        "/var/openebs"
      ];
      files = [
      ];
    };
    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d /persist/var/lib/containerd 755 root root"
      "d /persist/var/lib/rancher 755 root root"
      "d /persist/var/lib/cni 755 root root"
      "d /persist/var/lib/kubelet 755 root root"
      "d /persist/etc/rancher 755 root root"
      "d /persist/var/openebs 755 root root"
    ];
  };
}
