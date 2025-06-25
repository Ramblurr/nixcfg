{ config, lib, ... }:
let
  inherit (config.networking) hostName;
  inherit (lib.mori)
    keys
    filter
    first
    ;
  dhcpLib = import ./helpers.nix { inherit lib; };
  leaseOption = { };
  commonDhcpOptions = [
    {
      name = "domain-name-servers";
      data = dhcpLib.joinList config.repo.secrets.local.dns.internal;
    }
    {
      name = "time-servers";
      data = first config.site.net.lan0.hosts4.addams;
    }
  ];

  hostConfig = config.site.hosts.${hostName};
  dhcpInterfaces = keys (
    filter (net: iface: config.site.net.${net}.dhcp.enable) hostConfig.interfaces
  );

  # Helper function to generate a single VLAN subnet configuration
  mkSubnet =
    {
      net,
      netName,
      commonDhcpOptions,
      leaseOption,
    }:
    let
      ddnsEnabled = net ? domainName;
    in
    lib.mkIf net.dhcp.enable (
      {
        id = net.dhcp.id;
        interface = netName;
        subnet = net.subnet4;
        pools = [
          {
            pool = "${net.dhcp.start} - ${net.dhcp.end}";
          }
        ];
        hostname-char-set = "[^A-Za-z0-9.-]";
        hostname-char-replacement = "-";
        ddns-send-updates = ddnsEnabled;
        ddns-replace-client-name = "when-present";
        ddns-generated-prefix = "";
        ddns-qualifying-suffix = lib.mkIf ddnsEnabled "${net.domainName}.";
        option-data =
          [
            {
              name = "routers";
              data = first config.site.net.${netName}.hosts4.${net.dhcp.router};
            }
          ]
          ++ (
            if ddnsEnabled then
              [
                {
                  name = "domain-name";
                  data = net.domainName;
                }
              ]
            else
              [ ]
          )
          ++ (lib.my.mergeByKey "name" commonDhcpOptions net.dhcp.optionData);
        reservations = net.dhcp.reservations;
      }
      // leaseOption
    );

in
{
  environment.persistence."/persist".directories = [ "/var/lib/private/kea" ];
  services.kea.dhcp4 = {
    enable = true;
    settings = {
      lease-database = {
        name = "/var/lib/kea/kea-leases4.csv";
        persist = true;
        type = "memfile";
      };
      valid-lifetime = 86400;
      renew-timer = 3600;
      interfaces-config = {
        dhcp-socket-type = "raw";
        interfaces = lib.naturalSort dhcpInterfaces;
      };
      match-client-id = false;
      sanity-checks = {
        lease-checks = "fix-del";
      };
      subnet4 = (
        map (
          net:
          mkSubnet {
            netName = net;
            net = config.site.net.${net};
            inherit commonDhcpOptions leaseOption;
          }
        ) (lib.naturalSort dhcpInterfaces)
      );

    };
  };
}
