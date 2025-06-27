{ config, ... }:
let
  global = config.repo.secrets.global;
  domain = global.domain.home;

  # this is a /56 prefix with no trailing :
  prefix6 = config.repo.secrets.site.sitePrefix6;
in
{
  imports = [
    ../modules/site
  ];

  repo.secretFiles = {
    site = ../secrets/site.nix;
  };

  site.data = {
    contact = global.email.home;
    networkDomain = global.domain.home;
  };

  site.sshPubKeys = global.pubKeys;
  #site.net.wan6tun = {
  #  vlan = null;
  #  domainName = null;
  #  subnet4 = "192.168.254.0/24";
  #  subnets6 = { };
  #  #subnets6.main = "${prefix6}:0::/64";
  #  hosts4 = {
  #    addams = [ "192.168.254.1" ];
  #    wan6-gw = [ "192.168.254.2" ];
  #  };
  #  #hosts6.main = {
  #  #  addams = [ "${prefix6}:0::2" ];
  #  #  wan6-gw = [ "${prefix6}:0::1" ];
  #  #};
  #};
  site.net.lan0 = {
    # the untagged net
    vlan = null;
    domainName = "trunk.${domain}";
    subnet4 = "192.168.1.0/24";
    mtu = 9000;
    hosts4 = {
      addams = [
        "192.168.1.1"
        "192.168.1.3"
      ];
    };
    dhcp = {
      enable = true;
      id = 1;
      start = "192.168.1.200";
      end = "192.168.1.254";
      router = "addams";
    };
  };
  site.net.prim = {
    vlan = 4;
    domainName = "prim.${domain}";
    mtu = 1500;
    subnet4 = "10.9.4.0/22";
    subnets6 = { };
    #subnets6.main = "${prefix6}:4::/64";
    hosts4 = {
      addams = [
        "10.9.4.1"
        "10.9.4.4"
      ];
      debord = [ "10.9.4.21" ];
      dewey = [ "10.9.4.17" ];
      mali = [ "10.9.4.10" ];
    };
    hosts6 = { };
    #hosts6.main = {
    #  addams = [ "${prefix6}:4::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.9.6.10";
      end = "10.9.7.254";
      router = "addams";
    };
  };
  site.net.mgmt = {
    vlan = 9;
    domainName = "mgmt.${domain}";
    mtu = 1500;
    subnet4 = "10.9.8.0/23";
    subnets6 = { };
    #subnets6.main = "${prefix6}:9::/64";
    hosts4 = {
      addams = [ "10.9.8.1" ];
      quine = [ "10.9.8.33" ];
      dewey = [ "10.9.8.14" ];
      debord = [ "10.9.8.21" ];
      mali = [ "10.9.8.3" ];
    };
    #hosts6.main = {
    #  addams = [ "${prefix6}:9::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.9.9.200";
      end = "10.9.9.254";
      router = "addams";
    };
  };
  site.net.iot = {
    vlan = 50;
    domainName = "iot.${domain}";
    subnet4 = "10.8.50.0/23";
    subnets6 = { };
    #subnets6.main = "${prefix6}:50::/64";
    mtu = 1500;
    hosts4 = {
      addams = [ "10.8.50.1" ];
    };
    #hosts6.main = {
    #  addams = [ "${prefix6}:50::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.8.50.100";
      end = "10.8.50.254";
      router = "addams";
    };
  };
  site.net.inot = {
    vlan = 60;
    domainName = "not.${domain}";
    subnet4 = "10.8.60.0/23";
    subnets6 = { };
    #subnets6.main = "${prefix6}:60::/64";
    mtu = 1500;
    hosts4 = {
      addams = [ "10.8.60.1" ];
    };
    #hosts6.main = {
    #  addams = [ "${prefix6}:60::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.8.61.2";
      end = "10.8.61.254";
      router = "addams";
    };
  };
  site.net.data = {
    vlan = 11;
    domainName = "data.${domain}";
    subnet4 = "10.9.10.0/23";
    subnets6 = { };
    #subnets6.main = "${prefix6}:11::/64";
    mtu = 9000;
    hosts4 = {
      addams = [ "10.9.10.1" ];
      debord = [ "10.9.10.21" ];
      dewey = [ "10.9.10.14" ];
      mali = [ "10.9.10.10" ];
    };
    #hosts6.main = {
    #  addams = [ "${prefix6}:11::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.9.11.200";
      end = "10.9.11.254";
      router = "addams";
    };
  };
  site.net.guest = {
    vlan = 3;
    domainName = "guest.${domain}";
    subnet4 = "10.8.3.0/24";
    subnets6 = { };
    #subnets6.main = "${prefix6}:3::/64";
    mtu = 1500;
    hosts4 = {
      addams = [ "10.8.3.1" ];
    };
    #hosts6.main = {
    #  addams = [ "${prefix6}:3::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.8.3.6";
      end = "10.8.3.250";
      router = "addams";
    };
  };
  site.net.vpn = {
    vlan = 70;
    domainName = "vpn.${domain}";
    mtu = 1500;
    subnet4 = "10.8.70.0/24";
    subnets6 = { };
    #subnets6.main = "${prefix6}:70::/64";
    hosts4 = {
      addams = [ "10.8.70.1" ];
    };
    #hosts6.local = {
    #  addams = [ "${prefix6}:70::1" ];
    #};
    dhcp = {
      enable = true;
      start = "10.8.70.10";
      end = "10.8.70.254";
      router = "addams";
      optionData = [
        {
          name = "domain-name-servers";
          data = "10.64.0.1";
        }
      ];
    };
  };
  site.net.svc = {
    vlan = 5;
    domainName = "svc.${domain}";
    mtu = 1500;
    subnet4 = "172.20.20.0/24";
    subnets6 = { };
    #subnets6.main = "${prefix6}:5::/64";
    hosts4 = {
      addams = [ "172.20.20.1" ];
      quine = [ "172.20.20.2" ];
      dewey = [ "172.20.20.3" ];
      debord = [ "172.20.20.4" ];
    };
    #hosts6.local = {
    #  addams = [ "${prefix6}:5::1" ];
    #};
    dhcp = {
      enable = true;
      start = "172.20.20.200";
      end = "172.20.20.254";
      router = "addams";
    };
  };

  site.hosts = {
    addams = {
      role = "server";
      isRouter = true;
      interfaces = {
        lan0.type = "phys";
        #wan6tun = {
        #  type = "gre";
        #  routes = [
        #    {
        #      Destination = "::/0";
        #      Gateway = "${prefix6}:0::1";
        #      GatewayOnLink = true;
        #    }
        #  ];
        #};
        prim = {
          type = "bridge";
          routes = [
            {
              # allow return traffic to mali for borgmatic
              Destination = "10.9.4.10/32";
              Table = "vpn";
              Metric = 2;
            }
            {
              Destination = "192.168.8.0/22";
              Gateway = "10.9.4.21";
            }
          ];

        };
        mgmt.type = "bridge";
        iot.type = "bridge";
        inot.type = "bridge";
        data.type = "bridge";
        guest.type = "bridge";
        svc.type = "bridge";
        vpn = {
          type = "bridge";
          routingPolicyRules = [
            {
              Family = "both";
              IncomingInterface = "vpn";
              Table = "vpn";
              Priority = 100;
            }
          ];
        };
      };
    };
    debord = {
      role = "server";
      interfaces = {
        lan0.type = "phys";
        #lan1.type = "phys";
        prim = {
          type = "bridge";
          parent = "lan0";
        };
        data = {
          type = "bridge";
          parent = "lan1";
        };
        mgmt = {
          type = "bridge";
          parent = "lan0";
          gw4 = true;
        };
      };
    };
    dewey = {
      role = "server";
      interfaces = {
        lan0.type = "phys";
        #lan1.type = "phys";
        prim = {
          type = "bridge";
          parent = "lan0";
        };
        data = {
          type = "bridge";
          parent = "lan1";
        };
        mgmt = {
          type = "bridge";
          parent = "lan0";
          gw4 = true;
        };
        svc = {
          type = "bridge";
          parent = "lan0";
        };
      };
    };
    quine = {
      role = "server";
      interfaces = {
        lan0.type = "phys";
        prim.type = "bridge";
        mgmt.type = "bridge";
        svc.type = "bridge";
        vpn.type = "bridge";
      };
    };
    mali = {
      role = "server";
      interfaces = {
        prim = {
          type = "bridge";
          parent = "bond0";
          routes = [
            {
              Destination = "192.168.8.0/22";
              Gateway = "10.9.4.21";
            }
          ];
        };
        data = {
          type = "bridge";
          parent = "lan1";
          gw4 = true;
        };
        mgmt = {
          type = "bridge";
          parent = "bond0";
        };
      };
    };
  };
}
