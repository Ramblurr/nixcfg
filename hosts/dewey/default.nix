{
  config,
  lib,
  ...
}:
let
  inherit (config.networking) hostName;
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
      plex.enable = true;
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

  myhm = _: {
    home.persistence."/persist" = {
      directories = [ { directory = "work"; } ];
    };
  };

  # nixbot CI on debord: dewey terminates TLS for internal clients and for
  # the james gost tunnel, then proxies over the prim VLAN to debord.
  # See hosts/debord/nixbot.nix.
  modules.services.ingress.virtualHosts."ci.${config.repo.secrets.global.domain.work}" = {
    acmeHost = config.repo.secrets.global.domain.work;
    upstream = "http://debord.prim.${config.repo.secrets.global.domain.home}:${toString config.repo.secrets.home-ops.ports.nixbot}";
    upstreamExtraConfig = ''
      # GitHub webhook payloads can be up to 25 MB.
      client_max_body_size 25m;
      proxy_connect_timeout 120s;
      proxy_send_timeout 120s;
      # Long timeout keeps SSE log streams alive; buffering would stall SSE.
      proxy_read_timeout 3600s;
      proxy_buffering off;
    '';
  };

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
