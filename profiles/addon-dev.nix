{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../mixins/docker.nix
    ../mixins/podman.nix
  ];
  config = {
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.packages = with pkgs; [
        go
        terraform
        httpie
        openjdk11
        openjdk17
        openjdk19
        jetbrains.idea-ultimate
        jetbrains.datagrip
        jetbrains.gateway
        tcpdump
        docker-compose
        dig
        dogdns
        whois
        nmap
        yq
        k9s
        yamlfmt
        kustomize
        cilium-cli
        fluxcd
        esphome
      ];

      home.file.".ideavimrc" = {
        source = ../configs/ideavimrc;
      };
      home.file.".config/ideavim" = {
        source = ../configs/ideavim;
        recursive = true;
      };
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".config/JetBrains"
          ".cache/JetBrains"
          ".local/share/JetBrains"
          ".java/.userPrefs/jetbrains"
        ];
      };
    };
  };
}
