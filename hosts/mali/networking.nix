{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.networking) hostName;
in
{
  networking.usePredictableInterfaceNames = true;
  networking.firewall.allowPing = true;
  networking.nameservers = config.repo.secrets.global.nameservers;

  # Useful if you need to troubleshoot systemd-networkd
  # systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];

  site = config.repo.secrets.site.site;
  systemd.network =
    let
      hostConfig = config.site.hosts.${hostName};
      hostBridges = lib.naturalSort (
        lib.mori.keys (lib.mori.filter (_: iface: iface.type == "bridge") hostConfig.interfaces)
      );
      vlansForIface =
        thisName:
        (lib.mori.filter (
          bridgeName:
          (hostConfig.interfaces.${bridgeName}.parent != null)
          && (hostConfig.interfaces.${bridgeName}.parent == thisName)
        ) hostBridges);
    in
    {

      links = {
        "10-eno1" = {
          matchConfig.PermanentMACAddress = config.repo.secrets.local.eno1.mac;
          linkConfig.Name = "eno1";
        };
        "10-eno2" = {
          matchConfig.PermanentMACAddress = config.repo.secrets.local.eno2.mac;
          linkConfig.Name = "eno2";
        };
        "10-lan1" = {
          matchConfig.PermanentMACAddress = config.repo.secrets.local.lan1.mac;
          linkConfig.Name = "lan1";
        };
        "10-lan2" = {
          matchConfig.PermanentMACAddress = config.repo.secrets.local.lan2.mac;
          linkConfig.Name = "lan2";
        };
      };
      netdevs = {
        "10-bond0" = {
          netdevConfig = {
            Name = "bond0";
            Kind = "bond";
            #MTUBytes = "1500";
            Description = "Bond0 bonded interface";
          };
          bondConfig = {
            Mode = "802.3ad";
            MIIMonitorSec = "1s";
            LACPTransmitRate = "fast";
            UpDelaySec = "2s";
            DownDelaySec = "8s";
            TransmitHashPolicy = "layer3+4";
          };
        };
      };
      networks =
        # Disabled interfaces
        (lib.mori.merge (
          map
            (iface: {
              "10-${iface}" = {
                matchConfig.Name = iface;
                networkConfig.ConfigureWithoutCarrier = true;
                linkConfig = {
                  Unmanaged = "yes";
                  ActivationPolicy = "always-down";
                };
              };
            })
            [
              "lan2"
            ]
        ))
        // {
          "11-eno1" = {
            matchConfig.Name = "eno1";
            networkConfig.Bond = "bond0";
          };
          "11-eno2" = {
            matchConfig.Name = "eno2";
            networkConfig.Bond = "bond0";
          };
          "10-bond0" =
            let
            in
            {
              matchConfig.Name = "bond0";
              linkConfig.RequiredForOnline = "carrier";
              networkConfig = {
                BindCarrier = "eno1 eno2";
                DHCP = false;
                LinkLocalAddressing = false;
                VLAN = map (net: "vlan-${net}") (vlansForIface "bond0");
              };
            };
          "10-lan1" = {
            matchConfig.Name = "lan1";
            networkConfig = {
              DHCPServer = false;
              VLAN = map (net: "vlan-${net}") (vlansForIface "lan1");
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
