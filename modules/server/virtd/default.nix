{
  options,
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my;
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
in
{
  options.modules.server.virtd-host = {
    enable = mkBoolOpt false;
    zfsStorage = {
      enable = mkBoolOpt false;
      pool = mkOption {
        type = types.str;
        default = "rpool/encrypted/safe/vms";
        description = "The zfs pool to use for the storage pool";
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
      ++ (
        if cfg.zfsStorage.enable then
          [ "L+ /persist/var/lib/libvirt/storage/zfs-local.xml - - - - ${localZfsStorageXml}" ]
        else
          [ ]
      );
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
