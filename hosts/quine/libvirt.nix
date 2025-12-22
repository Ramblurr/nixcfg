{
  config,
  pkgs,
  ...
}:
let

  inherit (config.modules.users.primaryUser) username;
  localZfsStorageXml = pkgs.writeText "zfs-local.xml" ''
    <pool type='zfs'>
      <name>zfs-local</name>
      <uuid>fc626111-d7a2-4c1a-87e3-fb38f3b3f214</uuid>
      <capacity unit='bytes'>0</capacity>
      <allocation unit='bytes'>0</allocation>
      <available unit='bytes'>0</available>
      <source>
        <name>rpool/encrypted/safe/vms</name>
      </source>
      <target>
        <path>/dev/zvol/rpool/encrypted/safe/vms</path>
      </target>
    </pool>
  '';
  vpnVlanNetXml = pkgs.writeText "brvpn70.xml" ''
    <network>
      <name>brvpn70</name>
      <uuid>3e724900-e50e-43ae-ac5e-4ffd90d4ac5f</uuid>
      <forward mode='bridge'/>
      <bridge name='brvpn70'/>
    </network>
  '';

  primaryVlanNetXml = pkgs.writeText "brprim4.xml" ''
    <network>
      <name>brprim4</name>
      <uuid>6dea79f2-1512-40c6-a2a3-ed0b15a9c72d</uuid>
      <forward mode='bridge'/>
      <bridge name='brprim4'/>
    </network>
  '';
in
{

  systemd.tmpfiles.rules = [
    "d /persist/var/lib/libvirt 0775 root root -"
    "d /persist/var/lib/libvirt/storage 0775 root root -"
    "L+ /persist/var/lib/libvirt/qemu/networks/brvpn70.xml - - - - ${vpnVlanNetXml}"
    "L+ /persist/var/lib/libvirt/qemu/networks/brprim4.xml - - - - ${primaryVlanNetXml}"
    "L+ /persist/var/lib/libvirt/storage/zfs-local.xml - - - - ${localZfsStorageXml}"
  ];
  environment.persistence."/persist".directories = [ "/var/lib/libvirt" ];
  environment.persistence."/persist".users.${username}.directories = [
    ".config/virt-viewer"
  ];
  virtualisation.libvirtd = {
    enable = true;
    allowedBridges = [
      "virbr0"
      "brprim4"
      "brvpn70"
    ];
  };
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
  };

  boot.kernelModules = [ "vfio-pci" ];
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
