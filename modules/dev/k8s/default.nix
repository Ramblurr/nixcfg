{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.k8s;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.k8s = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ];
    myhm = {
      home.packages = with pkgs; [
        influxdb2-cli
        k9s
        kubernetes-helm
        kubectl
        kubeconform
        krew
        kustomize
        cilium-cli
        talosctl
        fluxcd
        cloudflared
      ];

      persistence = mkIf withImpermanence {
        directories = [
          ".config/k9s"
          ".config/kube"
          ".config/krew"
          ".influxdbv2"
        ];
      };
    };
  };
}
