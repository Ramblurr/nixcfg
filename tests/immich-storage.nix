{ pkgs, ... }:
let
  instance = import ../config/immich-home.nix;
  client = {
    boot.supportedFilesystems = [ "nfs" ];
    users.users.immich = {
      isSystemUser = true;
      inherit (instance) uid;
      group = "immich";
    };
    users.groups.immich.gid = instance.gid;
  };
in
pkgs.testers.runNixOSTest {
  name = "immich-crossmnt-storage";
  nodes = {
    api = client;
    worker = client;
    storage = { lib, nodes, ... }: {
      imports = [
        ../modules/zfs-attrs.nix
        ../hosts/mali/immich-home.nix
      ];
      options.site.net = lib.mkOption { type = lib.types.attrs; };
      config = {
        site.net = {
          data.hosts4.immich-home = [ nodes.api.networking.primaryIPAddress ];
          prim.hosts4.${instance.workerHost} = [ nodes.worker.networking.primaryIPAddress ];
        };
        networking.hostId = "12345678";
        boot.supportedFilesystems = [ "zfs" ];
        virtualisation.emptyDiskImages = [
          1024
          1024
        ];
        virtualisation.memorySize = 1536;
        modules.zfs.datasets.enable = true;
        services.nfs.server.enable = true;
        networking.firewall.allowedTCPPorts = [ 2049 ];
        systemd.services.test-pools = {
          requiredBy = [ "zfs-datasets.service" ];
          before = [ "zfs-datasets.service" ];
          after = [
            "zfs-mount.service"
            "local-fs.target"
          ];
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          path = [ pkgs.zfs ];
          script = ''
            zpool create -m none tank2 /dev/vdb
            zpool create -m none fast /dev/vdc
          '';
        };
      };
    };
  };
  testScript = ''
    root = "${instance.mediaExport}"
    derived = "${instance.derivedDataset}"
    directories = ["thumbs", "encoded-video"]
    start_all()
    storage.wait_for_unit("nfs-server.service")
    for client in (api, worker):
        client.succeed(f"mkdir -p /var/lib/immich; mount -t nfs -o vers=4.2 storage:{root} /var/lib/immich")

    with subtest("One NFS mount crosses both fast filesystems with matching service identity"):
        for directory in directories:
            assert storage.succeed(f"findmnt -n -o SOURCE --mountpoint {root}/{directory}").strip() == f"{derived}/{directory}"
            api.succeed(f"su -s /bin/sh immich -c 'echo api > /var/lib/immich/{directory}/probe'")
            worker.succeed(f"su -s /bin/sh immich -c 'grep -qx api /var/lib/immich/{directory}/probe; echo worker >> /var/lib/immich/{directory}/probe'")
            storage.succeed(f"grep -qx worker {root}/{directory}/probe")
            api.fail(f"touch /var/lib/immich/{directory}/root-squashed")
        api.succeed("su -s /bin/sh immich -c 'mkdir -p /var/lib/immich/upload; echo original > /var/lib/immich/upload/probe'")
        assert storage.succeed(f"findmnt -n -o SOURCE -T {root}/upload/probe").strip() == "${instance.mediaDataset}"

    with subtest("Mounted layout survives dataset reactivation"):
        storage.succeed("systemctl start zfs-datasets-reactivation.service")
        storage.succeed("systemctl is-active nfs-server.service")

    with subtest("Stopping a derived mount stops NFS instead of exposing shadow storage"):
        for client in (api, worker):
            client.succeed("umount /var/lib/immich")
        unit = storage.succeed(f"systemd-escape --path --suffix=mount {root}/thumbs").strip()
        storage.succeed(f"systemctl stop '{unit}'")
        storage.fail("systemctl is-active nfs-server.service")
        storage.fail("systemctl is-active immich-home-media-setup.service")
        storage.succeed(f"test -z \"$(find {root}/thumbs -mindepth 1 -print -quit)\"")

    with subtest("Activation refuses to cover unmigrated files, including hidden markers"):
        storage.succeed(f"touch {root}/thumbs/.immich")
        storage.fail("systemctl start zfs-datasets-reactivation.service")
        storage.succeed(f"test -f {root}/thumbs/.immich")
        storage.fail("systemctl start nfs-server.service")
        storage.fail(f"mountpoint -q {root}/thumbs")
        storage.succeed(f"rm {root}/thumbs/.immich; systemctl reset-failed zfs-datasets-reactivation.service")
        storage.succeed("systemctl start zfs-datasets-reactivation.service; systemctl start nfs-server.service")
        storage.succeed(f"grep -qx worker {root}/thumbs/probe")
        api.succeed(f"mount -t nfs -o vers=4.2 storage:{root} /var/lib/immich; su -s /bin/sh immich -c 'grep -qx worker /var/lib/immich/thumbs/probe'")
  '';
}
