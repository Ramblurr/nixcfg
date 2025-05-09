{ ... }:
{
  virtualisation = {
    containers.enable = true;
    oci-containers.backend = "podman";
    podman = {
      enable = true;
      defaultNetwork.settings = {
        network_interface = "ctr0";
        # setting this to true ironically breaks the dns because dns server is not running in the gateway ip
        # even setting `dns_name_servers` here doesn't work with this set to true
        dns_enabled = false;
        subnets = [
          # change these
          {
            subnet = "10.12.0.0/24";
            gateway = "10.12.0.1";
            lease_range = {
              start_ip = "10.12.0.50";
              end_ip = "10.12.0.200";
            };
          }
        ];
      };
    };
  };
}
