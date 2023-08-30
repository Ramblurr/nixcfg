{
  config,
  options,
  inputs,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.k8s;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.k8s = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
    ];
    myhm = {
      home.packages = with pkgs; [
        influxdb2-cli
        k9s
        kubernetes-helm
        kubectl
        krew
        kustomize
        cilium-cli
        talosctl
        fluxcd
        inputs.talhelper.packages.${pkgs.stdenv.hostPlatform.system}.default
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