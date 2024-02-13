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

    networking.firewall.allowedTCPPorts = [
      7472 # MetalLB (TCP+UDP)
      7473 # MetalLB FRR (TCP+UDP)
      7946 # MetalLB (TCP+UDP)
      10250 # K3s Metrics Server (TCP)
      10254 # MetalLB Metrics Export (TCP)
    ];
    networking.firewall.allowedUDPPorts = [
      7472 # MetalLB (TCP+UDP)
      7473 # MetalLB FRR (TCP+UDP)
      7946 # MetalLB (TCP+UDP)
    ];
    environment.persistence."/persist" = mkIf withImpermanence {
      hideMounts = true;
      directories = [
        "/var/lib/rancher"
        "/etc/rancher"
        "/var/lib/cni"
        "/var/openebs"
      ];
      files = [
      ];
    };
    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d /persist/var/lib/rancher 755 root root"
      "d /persist/var/lib/cni 755 root root"
      "d /persist/etc/rancher 755 root root"
      "d /persist/var/openebs 755 root root"
    ];
  };
}
