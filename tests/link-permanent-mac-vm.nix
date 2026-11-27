{ pkgs, ... }:
pkgs.testers.runNixOSTest {
  name = "link-permanent-mac";
  nodes.machine = {
    virtualisation.vlans = [ 1 ];
    networking.useDHCP = false;
    networking.useNetworkd = true;
    systemd.network = {
      enable = true;
      links."10-lan0" = {
        matchConfig.PermanentMACAddress = "52:54:00:12:01:01";
        linkConfig.Name = "lan0";
      };
      netdevs."20-vlan-svc" = {
        netdevConfig = {
          Name = "vlan-svc";
          Kind = "vlan";
        };
        vlanConfig.Id = 10;
      };
      networks."10-lan0" = {
        matchConfig.Name = "lan0";
        vlan = [ "vlan-svc" ];
        networkConfig.ConfigureWithoutCarrier = true;
      };
      networks."20-vlan-svc" = {
        matchConfig.Name = "vlan-svc";
        networkConfig.ConfigureWithoutCarrier = true;
      };
    };
  };
  testScript = ''
    machine.wait_for_unit("sys-subsystem-net-devices-lan0.device")
    machine.wait_until_succeeds("test -e /sys/class/net/vlan-svc", timeout=30)
    machine.wait_for_unit("sys-subsystem-net-devices-vlan\\x2dsvc.device", timeout=30)
    machine.fail("journalctl -b -o cat | grep 'Failed to rename network interface'")
  '';
}
