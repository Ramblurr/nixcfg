  { config, lib, pkgs, ... }: {
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
      ];
    };
  };
}
