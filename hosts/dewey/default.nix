{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (config.modules.users.primaryUser) homeDirectory;
  defaultSopsFile = ./secrets.sops.yaml;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ./guests.nix
    #./networking.nix
    ../../config
    ../../config/home-ops.nix
    ../../modules/site-net
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  modules.vpn.tailscale.enable = true;
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  networking.firewall.allowedTCPPorts = [
    5432
  ];
  home-ops = {
    enable = true;
    ingress.enable = true;
    postgresql = {
      enable = true;
      onsiteBackup.enable = false;
      offsiteBackup.enable = false;
    };
    mariadb.enable = true;
    containers.enable = true;
    apps = {
      roon-server.enable = true;
      onepassword-connect.enable = true;
      authentik.enable = true;
      davis.enable = true;
      invoiceninja.enable = true;
      paperless.enable = true;
      ocis-work.enable = true;
      plex.enable = true;
      home-dl.enable = true;
      calibre.enable = true;
      calibre-web.enable = true;
      archivebox.enable = false;
      #linkding.enable = true;
      matrix-synapse.enable = true;
      influxdb.enable = true;
      git-archive.enable = true;
      forgejo.enable = false;
      actual-server.enable = false;
      atuin-sync.enable = true;
      snowflake-proxy.enable = true;
    };
  };

  myhm =
    { pkgs, ... }@hm:
    {
      home.persistence."/persist${homeDirectory}" = {
        directories = [ { directory = "work"; } ];
      };
    };

  site = config.repo.secrets.site.site;
  systemd.network = {
    links = {
      "10-lan0" = {
        matchConfig.MACAddress = config.repo.secrets.site.site.hosts.dewey.interfaces.lan0.hwaddr;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        matchConfig.MACAddress = config.repo.secrets.local.lan1.hwaddr;
        linkConfig.Name = "lan1";
      };
    };
    networks = {
      "10-lan1" =
        let

          hostConfig = config.site.hosts.${hostName};
          hostBridges = lib.naturalSort (
            lib.mori.keys (lib.mori.filter (_: iface: iface.type == "bridge") hostConfig.interfaces)
          );
          vlansForThisIface = (
            lib.mori.filter (
              bridgeName:
              (hostConfig.interfaces.${bridgeName}.parent != null)
              && (hostConfig.interfaces.${bridgeName}.parent == "lan1")
            ) hostBridges
          );
        in
        {
          matchConfig.Name = "lan1";
          networkConfig = {
            DHCPServer = false;
            VLAN = map (net: "vlan-${net}") vlansForThisIface;
            LinkLocalAddressing = false;
            LLDP = true;
            EmitLLDP = true;
            Description = "I am the 10gbe sfp+ link";
          };
          linkConfig = {
            MTUBytes = 9000;
            RequiredForOnline = "carrier";
          };
        };
    };
  };
}
