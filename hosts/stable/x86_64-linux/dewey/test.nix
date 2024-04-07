{
  config,
  lib,
  pkgs,
  inputs,
  hn,
  ...
}:

{

  networking.domain = "socozy.casa";
  repo.secretFiles.home-ops = ../../../../secrets/home-ops.nix;
  modules.zfs.datasets.enable = true;

  home-ops = {
    enable = true;
    ingress.enable = true;
    postgresql = {
      enable = true;
      onsiteBackup.enable = true;
    };
    mariadb.enable = true;
    containers.enable = true;
    apps = {
      #authentik.enable = true;
      #echo-server.enable = true;
      davis.enable = true;
      #invoiceninja.enable = true;
      paperless.enable = true;
      ocis-work.enable = true;
      plex.enable = false;
      tautulli.enable = true;
    };
  };
}
