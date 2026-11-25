{ inputs, pkgs }:
let
  siteLib =
    (import inputs.nixpkgs {
      inherit (pkgs.stdenv.hostPlatform) system;
      overlays = [ (import ../lib inputs) ];
    }).lib;

  site = {
    site.data = {
      contact = "ops@example.test";
      networkDomain = "example.test";
    };
    site.net = {
      eth1 = {
        vlan = null;
        mtu = 1500;
      };
      prim = {
        vlan = 4;
        mtu = 1500;
        subnet4 = "10.9.4.0/22";
        hosts4 = {
          router = [ "10.9.4.1" ];
          plain = [ "10.9.4.10" ];
          fixed = [ "10.9.4.11" ];
        };
        dhcp = {
          enable = true;
          start = "10.9.6.10";
          end = "10.9.6.20";
          router = "router";
        };
      };
      mgmt = {
        vlan = 9;
        mtu = 1500;
        subnet4 = "10.9.8.0/23";
        hosts4 = {
          router = [ "10.9.8.1" ];
          plain = [ "10.9.8.3" ];
          fixed = [ "10.9.8.4" ];
          client = [ "10.9.8.19" ];
        };
        dhcp = {
          enable = true;
          start = "10.9.9.200";
          end = "10.9.9.210";
          router = "router";
        };
      };
    };
    site.hosts =
      let
        twoLegs = {
          role = "server";
          interfaces = {
            eth1.type = "phys";
            prim.type = "bridge";
            mgmt = {
              type = "bridge";
              gw4 = true;
            };
          };
        };
      in
      {
        router.isRouter = true;
        plain = twoLegs // {
          sourceRouting = false;
        };
        fixed = twoLegs;
        client = {
          role = "server";
          interfaces = {
            eth1.type = "phys";
            mgmt = {
              type = "bridge";
              gw4 = true;
            };
          };
        };
      };
  };

  siteNode =
    { lib, ... }:
    {
      imports = [
        ../modules/site
        ../modules/site-net
        site
      ];
      options.repo.secrets = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
      config = {
        virtualisation.vlans = [ 1 ];
        networking.interfaces = lib.mkForce { };
        networking.firewall.enable = false;
        services.timesyncd.enable = lib.mkForce false;
        environment.systemPackages = [ pkgs.python3 ];
        systemd.services.http = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig.ExecStart = "${pkgs.python3}/bin/python -m http.server 8000 --directory /etc";
        };
      };
    };

  probe = pkgs.writeText "probe.py" ''
    import socket, sys
    s = socket.create_connection((sys.argv[1], 8000), timeout=3)
    print("connected", flush=True)
    s.settimeout(5)
    s.sendall(b"GET /hostname HTTP/1.0\r\n\r\n")
    try:
        data = s.recv(4096)
        print("reply", data.split(b"\r\n", 1)[0].decode())
    except socket.timeout:
        print("stalled")
        sys.exit(2)
  '';
in
pkgs.testers.runNixOSTest {
  name = "site-net-source-routing";
  node.specialArgs.lib = siteLib;

  nodes = {
    router = {
      virtualisation.vlans = [ 1 ];
      networking.useDHCP = false;
      networking.interfaces = pkgs.lib.mkForce { };
      networking.firewall.enable = false;
      networking.nftables = {
        enable = true;
        tables.fw = {
          family = "inet";
          content = ''
            chain forward {
              type filter hook forward priority filter; policy drop;
              ct state established,related accept
              ct state invalid counter drop
              iifname { "prim", "mgmt" } oifname { "prim", "mgmt" } ct state new accept
            }
          '';
        };
      };
      systemd.network = {
        enable = true;
        config.networkConfig.IPv4Forwarding = true;
        netdevs = {
          "20-prim" = {
            netdevConfig = {
              Kind = "vlan";
              Name = "prim";
            };
            vlanConfig.Id = 4;
          };
          "20-mgmt" = {
            netdevConfig = {
              Kind = "vlan";
              Name = "mgmt";
            };
            vlanConfig.Id = 9;
          };
        };
        networks = {
          "10-eth1" = {
            matchConfig.Name = "eth1";
            networkConfig = {
              VLAN = [
                "prim"
                "mgmt"
              ];
              LinkLocalAddressing = false;
            };
          };
          "30-prim" = {
            matchConfig.Name = "prim";
            address = [ "10.9.4.1/22" ];
          };
          "30-mgmt" = {
            matchConfig.Name = "mgmt";
            address = [ "10.9.8.1/23" ];
          };
        };
      };
    };
    plain =
      { lib, ... }:
      {
        imports = [ siteNode ];
        networking.hostName = "plain";
        specialisation.routed.configuration.site.hosts.plain.sourceRouting = lib.mkForce true;
      };
    fixed = {
      imports = [ siteNode ];
      networking.hostName = "fixed";
    };
    client = {
      imports = [ siteNode ];
      networking.hostName = "client";
    };
  };

  testScript = ''
    start_all()
    for m in (router, plain, fixed, client):
        m.wait_for_unit("multi-user.target")
    for m in (plain, fixed, client):
        m.wait_for_unit("http.service")
        m.wait_for_open_port(8000)
    router.wait_until_succeeds("ping -c1 -W1 10.9.8.19")
    router.wait_until_succeeds("ping -c1 -W1 10.9.4.10")
    router.wait_until_succeeds("ping -c1 -W1 10.9.4.11")

    print(fixed.succeed("ip rule; ip route show table 1004; ip route show table 1009"))
    print(plain.succeed("ip rule"))
    print(client.succeed("ip rule"))
    client.fail("ip rule | grep -q 'lookup 100[49]'")
    plain.fail("ip rule | grep -q 'lookup 100[49]'")
    fixed.succeed("ip rule | grep -q 'from 10.9.4.0/22 to 10.9.8.0/23 lookup 1004'")
    fixed.succeed("ip rule | grep -q 'from 10.9.8.0/23 to 10.9.4.0/22 lookup 1009'")
    fixed.succeed("ip route get 10.9.8.19 from 10.9.4.11 | grep -q 'via 10.9.4.1 dev prim'")
    fixed.succeed("ip route get 10.9.8.19 | grep -q 'dev mgmt src 10.9.8.4'")
    fixed.succeed("ip route get 10.9.8.19 from 10.9.8.4 | grep -q 'dev mgmt'")
    fixed.fail("ip route get 10.9.8.19 from 10.9.8.4 | grep -q via")
    plain.succeed("ip route get 10.9.8.19 from 10.9.4.10 | grep -q 'dev mgmt'")

    with subtest("without source routing the cross-VLAN reply bypasses the router and the flow stalls"):
        out = client.fail("python3 ${probe} 10.9.4.10")
        print(out)
        assert "connected" in out and "stalled" in out, out
        print(router.succeed("nft list chain inet fw forward"))

    with subtest("with source routing the reply goes back through the router and the flow completes"):
        out = client.succeed("python3 ${probe} 10.9.4.11")
        print(out)
        assert "reply HTTP/1.0 200 OK" in out, out
        print(client.succeed("curl -sf --max-time 5 http://10.9.4.11:8000/hostname"))

    with subtest("same-subnet traffic stays direct and works both ways"):
        for addr in ("10.9.8.3", "10.9.8.4"):
            client.succeed(f"curl -sf --max-time 5 http://{addr}:8000/hostname")
        for m in (plain, fixed):
            m.succeed("curl -sf --max-time 5 http://10.9.8.19:8000/hostname")
        fixed.succeed("curl -sf --max-time 5 --interface 10.9.8.4 http://10.9.8.19:8000/hostname")

    with subtest("traffic from a bound prim address to the client goes through the router and works"):
        fixed.succeed("curl -sf --max-time 5 --interface 10.9.4.11 http://10.9.8.19:8000/hostname")

    with subtest("switching a running host to source routing keeps foreign addresses, routes and rules"):
        plain.succeed("ip addr add 10.9.8.22/32 dev mgmt proto 18")
        plain.succeed("ip route replace default via 10.9.4.1 dev prim table 100")
        plain.succeed("ip route replace 10.9.4.0/22 dev prim table 100")
        plain.succeed("ip rule add priority 100 from 192.168.10.8/29 table 100")
        plain.succeed("/run/current-system/specialisation/routed/bin/switch-to-configuration test")
        plain.wait_until_succeeds("ip rule | grep -q 'from 10.9.4.0/22 to 10.9.8.0/23 lookup 1004'")
        print(plain.succeed("ip rule; ip -4 addr show mgmt; ip route show table 100; journalctl -u systemd-networkd --no-pager | tail -30"))
        out = client.succeed("python3 ${probe} 10.9.4.10")
        assert "reply HTTP/1.0 200 OK" in out, out
        plain.succeed("ip -4 addr show mgmt | grep -q 10.9.8.22/32")
        plain.succeed("ip route show table 100 | grep -q 'default via 10.9.4.1 dev prim'")
        plain.succeed("ip route show table 100 | grep -q '10.9.4.0/22 dev prim'")
        plain.succeed("ip rule | grep -q '^100:.*from 192.168.10.8/29 lookup 100'")
  '';
}
