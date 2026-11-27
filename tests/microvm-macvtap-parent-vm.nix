{ inputs, pkgs }:
pkgs.testers.runNixOSTest {
  name = "microvm-macvtap-parent";

  nodes.machine =
    { lib, ... }:
    {
      imports = [
        inputs.microvm.nixosModules.host
        ../modules/motd.nix
        ../modules/microvm-host
      ];

      options.environment.persistence = lib.mkOption {
        type = lib.types.anything;
        default = { };
      };

      config = {
        system.stateVersion = "26.05";
        virtualisation.vlans = [ ];
        modules.microvm-host = {
          enable = true;
          baseZfsDataset = "rpool/test";
        };

        networking.useNetworkd = true;
        system.switch.enable = true;
        specialisation.changed.configuration.environment.etc.changed.text = "1";
        systemd.network = {
          wait-online = {
            anyInterface = false;
            timeout = 15;
          };
          netdevs = {
            "20-vlan-svc" = {
              netdevConfig = {
                Kind = "vlan";
                Name = "vlan-svc";
              };
              vlanConfig.Id = 8;
            };
            "30-dark0" = {
              netdevConfig = {
                Kind = "veth";
                Name = "dark0";
              };
              peerConfig.Name = "dark0p";
            };
          };
          networks = {
            "10-trunk0" = {
              matchConfig.Name = "trunk0";
              vlan = [ "vlan-svc" ];
              networkConfig.LinkLocalAddressing = "no";
            };
            "20-vlan-svc" = {
              matchConfig.Name = "vlan-svc";
              networkConfig.LinkLocalAddressing = "no";
            };
            "30-dark0" = {
              matchConfig.Name = "dark0";
              address = [ "192.0.2.1/24" ];
            };
            "30-dark0p" = {
              matchConfig.Name = "dark0p";
              linkConfig.Unmanaged = true;
            };
          };
        };
        systemd.targets.network-online.wantedBy = [ "multi-user.target" ];

        microvm.vms.ward.config = {
          options.modules.microvm-guest = lib.mkOption {
            type = lib.types.anything;
            default = { };
          };
          config.system.stateVersion = "26.05";
          config.microvm.interfaces = [
            {
              type = "macvtap";
              id = "svc-ward";
              mac = "02:00:00:00:00:01";
              macvtap = {
                link = "vlan-svc";
                mode = "bridge";
              };
            }
          ];
        };
      };
    };

  testScript = ''
    import time

    unit = "microvm-macvtap-interfaces@ward.service"

    def state(name):
        return machine.succeed(f"systemctl show -p ActiveState --value '{name}'").strip()

    with subtest("the MACVTAP setup waits while the parent link is missing"):
        machine.wait_until_succeeds("systemctl is-failed systemd-networkd-wait-online.service", timeout=120)
        machine.fail("ip link show vlan-svc")
        machine.fail("journalctl -b -u " + unit + " | grep -q 'does not exist'")
        assert state(unit) != "active"
        assert state("microvm@ward.service") != "active"

    with subtest("a switch while the parent link is missing completes"):
        started = time.monotonic()
        status, output = machine.execute(
            "timeout 300 /run/booted-system/specialisation/changed/bin/switch-to-configuration test 2>&1"
        )
        elapsed = time.monotonic() - started
        print(output[-2000:])
        print(f"switch: status {status} after {elapsed:.1f}s")
        assert status != 124 and elapsed < 60, f"switch status {status} after {elapsed:.1f}s"
        machine.succeed("test -e /etc/changed")

    with subtest("a manual guest start while the parent link is missing returns"):
        started = time.monotonic()
        status, _ = machine.execute("timeout 600 systemctl start microvm@ward.service")
        elapsed = time.monotonic() - started
        print(f"systemctl start microvm@ward: status {status} after {elapsed:.1f}s")
        assert status != 124, f"status {status} after {elapsed:.1f}s"
        assert state("microvm@ward.service") != "active"

    with subtest("a late parent link brings up the guest"):
        machine.fail("ip link show vlan-svc")
        machine.succeed("ip link add trunk0 type dummy")
        machine.wait_until_succeeds("ip link show vlan-svc")
        alias = machine.succeed(
            "systemd-escape --path --suffix=device /sys/subsystem/net/devices/vlan-svc"
        ).strip()
        machine.wait_until_succeeds(f"systemctl is-active '{alias}'", timeout=30)
        machine.wait_until_succeeds(f"systemctl is-active {unit}", timeout=60)
        machine.succeed("ip link show svc-ward")
        machine.wait_until_succeeds("systemctl is-active microvm@ward.service", timeout=120)
  '';
}
