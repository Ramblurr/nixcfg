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
  ramblurr = import ../ramblurr.nix {
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
    ./guests.nix
    ../../config/secrets.nix
    ../../config/home-ops.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  modules.networking.default.hostName = hn;

  modules.vpn.tailscale.enable = true;
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
      archivebox.enable = true;
      linkding.enable = true;
      matrix-synapse.enable = true;
      influxdb.enable = true;
      git-archive.enable = true;
      forgejo.enable = true;
      actual-server.enable = true;
      atuin-sync.enable = true;
      soju.enable = true;
      snowflake-proxy.enable = true;
    };
  };

  myhm =
    { pkgs, ... }@hm:
    {
      home.persistence."/persist${ramblurr.homeDirectory}" = {
        directories = [ { directory = "work"; } ];
      };
    };
}
