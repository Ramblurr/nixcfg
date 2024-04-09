{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hn = "dewey";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {
    inherit
      config
      lib
      pkgs
      inputs
      ;
  };
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ../../../../modules/_config/secrets.nix
    ../../../../modules/_config/home-ops.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  modules.networking.default.hostName = hn;

  home-ops = {
    enable = true;
    user = ramblurr;
    ingress.enable = true;
    postgresql = {
      enable = true;
      onsiteBackup.enable = true;
    };
    mariadb.enable = true;
    containers.enable = true;
    #hypervisor.enable = false;
    apps = {
      roon-server.enable = true;
      onepassword-connect.enable = true;
      authentik.enable = true;
      #echo-server.enable = true;
      davis.enable = true;
      invoiceninja.enable = true;
      paperless.enable = true;
      ocis-work.enable = true;
      plex.enable = true;
      #tautulli.enable = true;
      home-dl.enable = true;
      calibre.enable = true;
      calibre-web.enable = true;
    };
  };
  #myhm = {
  #  persistence = {
  #    directories = [ "dumps" ];
  #  };
  #};
}
