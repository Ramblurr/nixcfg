{
  config,
  lib,
  pkgs,
  ...
}:
{
  virtualisation.libvirtd = {
    enable = true;
    allowedBridges = [
      "virbr0"
      "brprim4"
    ];
  };
  #services.nfs.server.enable = true;

  # Add firewall exception for VirtualBox provider
  #networking.firewall.extraCommands = ''
  #  ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  #'';

  # Add firewall exception for libvirt provider when using NFSv4
  #networking.firewall.interfaces."virbr1" = {
  #  allowedTCPPorts = [ 2049 ];
  #  allowedUDPPorts = [ 2049 ];
  #};
}
