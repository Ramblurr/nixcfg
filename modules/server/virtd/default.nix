{
  options,
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.server.virtd-host;
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
  # This is the network that will be used to allow guests to get an ip on my primary vlan
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
  options.modules.server.virtd-host = {
    enable = lib.mkEnableOption "";
    storage = {
      zfs = {
        enable = lib.mkEnableOption "Enable the zfs storage pool";
        pool = lib.mkOption {
          type = lib.types.str;
          default = "rpool/encrypted/safe/vms";
          description = "The zfs pool to use for the storage pool";
        };
      };
    };
    net = {
      brprim4 = {
        enable = lib.mkEnableOption "Enable the brprim4 network";
      };
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.impermanence.enable;
        message = "virtd-host requires impermanence";
      }
    ];
    systemd.tmpfiles.rules =
      [
        "d /persist/var/lib/libvirt 0775 root root -"
        "d /persist/var/lib/libvirt/storage 0775 root root -"
      ]
      ++ lib.optionals cfg.net.brprim4.enable [
        "L+ /persist/var/lib/libvirt/qemu/networks/brprim4.xml - - - - ${primaryVlanNetXml}"
      ]
      ++ lib.optionals cfg.storage.zfs.enable [
        "L+ /persist/var/lib/libvirt/storage/zfs-local.xml - - - - ${localZfsStorageXml}"
      ];
    environment.persistence."/persist".directories = [ "/var/lib/libvirt" ];

    virtualisation.libvirtd = {
      enable = true;
    };
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = true;
      #"net.ipv6.conf.all.forwarding" = true;
    };
    networking.firewall = {
      trustedInterfaces = [ "virbr0" ];
      # Needed for host <-> vm communication
      checkReversePath = lib.mkForce false;
    };
    boot.kernelModules = [ "vfio-pci" ];
  };
}
