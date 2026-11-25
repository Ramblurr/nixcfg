{ pkgs, ... }:
let
  inherit (pkgs) lib;
  fixture = pkgs.runCommand "immich-upload-fixture" { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
    magick -size 128x128 gradient:red-blue "$out.png"
    mv "$out.png" "$out"
  '';
  identity = {
    users.users.immich = {
      isSystemUser = true;
      uid = 3024;
      group = "immich";
    };
    users.groups.immich.gid = 3024;
  };
  client = { nodes, ... }: {
    imports = [ identity ];
    virtualisation.fileSystems."/var/lib/immich" = {
      device = "${nodes.storage.networking.primaryIPAddress}:/exports/immich";
      fsType = "nfs";
      options = [
        "vers=4.2"
        "hard"
        "_netdev"
        "nofail"
      ];
    };
    # Synthetic credentials for these disposable VMs only, never deployment data.
    environment.etc."immich-test/environment".text = ''
      DB_PASSWORD=immich-test-database
      REDIS_PASSWORD=immich-test-redis
    '';
    environment.etc."immich-test/redis-password".text = "immich-test-redis";
    environment.systemPackages = [
      pkgs.curl
      pkgs.jq
    ];
    virtualisation.memorySize = 3072;
    specialisation.switched.configuration.environment.etc."immich-test/generation".text = "switched";
  };
in
pkgs.testers.runNixOSTest {
  name = "immich-split-worker";
  nodes = {
    storage = { nodes, ... }: {
      imports = [ identity ];
      services.nfs.server = {
        enable = true;
        exports = ''
          /exports/immich ${nodes.api.networking.primaryIPAddress}(rw,sync,root_squash,no_subtree_check) ${nodes.worker.networking.primaryIPAddress}(rw,sync,root_squash,no_subtree_check)
        '';
      };
      systemd.tmpfiles.rules = [ "d /exports/immich 0700 immich immich -" ];
      networking.firewall.allowedTCPPorts = [ 2049 ];
    };
    api = { nodes, ... }: {
      imports = [
        client
        ../modules/motd.nix
        ../guests/immich-home/immich.nix
      ];
      options = {
        site.net = lib.mkOption { type = lib.types.attrs; };
      };
      config = {
        modules.services.immich-ml-proxy.enable = lib.mkForce false;
        services.immich.secretsFile = lib.mkForce "/etc/immich-test/environment";
        services.redis.servers.immich.requirePassFile = lib.mkForce "/etc/immich-test/redis-password";
        systemd.services.postgresql-setup.serviceConfig.EnvironmentFile =
          lib.mkForce "/etc/immich-test/environment";
        site.net.svc.hosts4 = {
          api = [ nodes.api.networking.primaryIPAddress ];
          ${(import ../config/immich-home.nix).workerHost} = [ nodes.worker.networking.primaryIPAddress ];
          dewey = [ nodes.api.networking.primaryIPAddress ];
        };
        site.net.prim.hosts4 = {
          api = [ nodes.api.networking.primaryIPAddress ];
          ${(import ../config/immich-home.nix).workerHost} = [ nodes.worker.networking.primaryIPAddress ];
        };
        networking.nftables.enable = true;
      };
    };
    worker = { nodes, ... }: {
      imports = [
        client
        ../modules/motd.nix
        ../modules/services/immich-worker.nix
      ];
      modules.services.immich-worker.enable = true;
      services.immich = {
        # Exercise real non-GPU jobs without CUDA builds or model downloads.
        package = pkgs.immich;
        accelerationDevices = lib.mkForce [ ];
        machine-learning.enable = lib.mkForce false;
        database.host = nodes.api.networking.primaryIPAddress;
        redis = {
          host = nodes.api.networking.primaryIPAddress;
          port = 6379;
        };
        secretsFile = "/etc/immich-test/environment";
      };
    };
  };
  testScript = ''
    import json
    import shlex

    storage.start()
    storage.wait_for_unit("nfs-server.service")
    storage.succeed("systemctl stop nfs-server.service")
    start_all()

    def boot_timeline(machine):
        machine.wait_for_unit("multi-user.target", timeout=180)
        machine.fail("systemctl is-active immich-server.service")
        timeout = "journalctl -b -o json -u var-lib-immich.mount --grep \"Failed with result 'timeout'\""
        machine.wait_until_succeeds(timeout, timeout=120)
        reached = int(machine.succeed("systemctl show multi-user.target -p ActiveEnterTimestampMonotonic --value"))
        failed = int(machine.succeed(timeout + " | jq -rs '.[0].__MONOTONIC_TIMESTAMP'"))
        machine.log(f"multi-user.target at {reached / 1e6:.2f} s, first mount timeout at {failed / 1e6:.2f} s")
        return reached, failed

    with subtest("Both roles reach multi-user before the first NFS mount attempt times out"):
        timelines = {machine.name: boot_timeline(machine) for machine in [api, worker]}
        assert all(reached < failed for reached, failed in timelines.values()), timelines

    with subtest("Both roles start Immich by themselves once NFS returns"):
        storage.succeed("systemctl start nfs-server.service")
        for machine in [api, worker]:
            machine.wait_for_unit("immich-server.service", timeout=300)
        api.wait_until_succeeds("curl -fsS http://api:2283/api/server/ping")

    with subtest("A switch starts a stopped Immich while its mount stays active"):
        mounted = "systemctl show var-lib-immich.mount -p ActiveEnterTimestampMonotonic --value"
        for machine in [api, worker]:
            machine.succeed("systemctl stop immich-server.service")
            machine.succeed("systemctl is-active var-lib-immich.mount")
            before = machine.succeed(mounted)
            machine.succeed("/run/current-system/specialisation/switched/bin/switch-to-configuration test")
            machine.wait_for_unit("immich-server.service", timeout=60)
            assert machine.succeed(mounted) == before, "the switch remounted var-lib-immich.mount"

    api.wait_until_succeeds("curl -fsS http://api:2283/api/server/ping")
    worker.succeed("systemctl stop immich-server.service")

    def request(method, path, body=None, token=None):
        command = f"curl -fsS -X {method} http://api:2283/api/{path}"
        if body is not None:
            command += " -H 'Content-Type: application/json' --data " + shlex.quote(json.dumps(body))
        if token:
            command += " -H " + shlex.quote("Authorization: Bearer " + token)
        return json.loads(api.succeed(command))

    with subtest("API serves without a background worker"):
        account = {"email": "admin@example.test", "password": "immich-test-admin-password", "name": "Test Admin"}
        request("POST", "auth/admin-sign-up", account)
        token = request("POST", "auth/login", account)["accessToken"]
        settings = request("GET", "system-config", token=token)
        settings["machineLearning"]["enabled"] = False
        request("PUT", "system-config", settings, token)
        assert request("GET", "system-config", token=token)["machineLearning"]["enabled"] is False

    with subtest("Upload queues real work while worker is stopped"):
        auth = shlex.quote("Authorization: Bearer " + token)
        upload = json.loads(api.succeed(
            "curl -fsS http://api:2283/api/assets -H " + auth +
            " -F deviceAssetId=fixture -F deviceId=nixos-test"
            " -F fileCreatedAt=2026-09-10T00:00:00.000Z -F fileModifiedAt=2026-09-10T00:00:00.000Z"
            " -F 'assetData=@${fixture};filename=fixture.png;type=image/png'"
        ))
        asset_id = upload["id"]
        assert request("GET", "assets/" + asset_id, token=token)["thumbhash"] is None

    with subtest("Remote worker processes the queued work on shared NFS"):
        worker.succeed("systemctl start immich-server.service")
        worker.wait_for_unit("immich-server.service")
        api.wait_until_succeeds(
            "curl -fsS http://api:2283/api/assets/" + asset_id + " -H " + auth +
            " | jq -e '.thumbhash != null and .exifInfo != null'", timeout=180
        )
        worker.fail("systemctl is-active postgresql.service")
        worker.fail("systemctl is-active immich-machine-learning.service")

    with subtest("Mount loss stops the worker instead of writing to local shadow storage"):
        worker.succeed("systemctl stop var-lib-immich.mount")
        worker.fail("systemctl is-active immich-server.service")
        worker.succeed("test -z \"$(find /var/lib/immich -mindepth 1 -print -quit)\"")
  '';
}
