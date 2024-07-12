{
  config,
  globals,
  nodes,
  pkgs,
  ...
}:
let
in
{
  microvm.mem = 1024;
  microvm.vcpu = 2;
  networking.firewall.allowedTCPPorts = [ 22 ];
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
  };
  users.users.root = {
    openssh.authorizedKeys.keys = config.modules.users.primaryUser.authorizedKeys;
  };
  systemd.network.enable = true;
  networking.useNetworkd = true;
  systemd.network.networks."20-lan" = {
    matchConfig.Type = "ether";
    networkConfig = {
      DHCP = "yes";
      ##IPForward = "yes";
      #DNSSEC = "no";
      #Address = [ "10.9.4.201/22" ];
      #Gateway = "10.9.4.1";
      #DNS = [ "10.9.4.4" ];
      #IPv6AcceptRA = true;
      #DHCP = "no";
    };
  };

}
