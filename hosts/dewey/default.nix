{
  config,
  lib,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (config.modules.users.primaryUser) username;
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
    ./ingress.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  users.users.${username}.linger = true;

  modules.vpn.tailscale.enable = true;
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  networking.firewall.allowedTCPPorts = [
    # todo: after microvm migration restrict with nftables to svc zone
    5432
    3306
  ];
  networking.firewall.interfaces.mgmt.allowedTCPPorts = [
    config.modules.services.ingress.directWan.listenPort
  ];
  networking.firewall.interfaces.prim.allowedTCPPorts = [ 8096 ];
  modules.services.ingress.directWan = {
    enable = true;
    listenAddress = builtins.head config.site.net.mgmt.hosts4.${hostName};
  };
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
      audiobookshelf.enable = true;
      filebrowser-quantum.enable = true;
      roon-server.enable = true;
      authentik.enable = true;
      davis.enable = true;
      paperless.enable = true;
      ocis-work.enable = true;
      jellyfin.enable = true;
      home-dl.enable = true;
      calibre.enable = true;
      calibre-web.enable = true;
      koreader-sync.enable = true;
      #archivebox.enable = true;
      matrix-synapse.enable = true;
      influxdb.enable = true;
      git-archive.enable = true;
      forgejo.enable = true;
      #actual-server.enable = true;
      atuin-sync.enable = true;
      snowflake-proxy.enable = true;
      my-y2r.enable = true;
      tubearchivist.enable = true;
      invoiceninja.enable = true;
      stirling-pdf.enable = true;
    };
  };

  environment.persistence."/persist".users.${username}.directories = [ "work" ];

  modules.services.ingress-nixbot.enable = true;
  modules.services.ingress-paseo.enable = true;
  modules.services.ingress-phoniebox.enable = true;

  inherit (config.repo.secrets.site) site;
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
          vlansForThisIface = lib.mori.filter (
            bridgeName:
            (hostConfig.interfaces.${bridgeName}.parent != null)
            && (hostConfig.interfaces.${bridgeName}.parent == "lan1")
          ) hostBridges;
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
