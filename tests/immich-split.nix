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
        ../guests/immich-home/immich.nix
      ];
      options = {
        repo.secrets = lib.mkOption { type = lib.types.attrs; };
        site.net = lib.mkOption { type = lib.types.attrs; };
      };
      config = {
        repo.secrets.local.immich = {
          workerHost = "worker";
          mediaLocation = "/var/lib/immich";
          secretsDirectory = "/etc/immich-test";
        };
        site.net.svc.hosts4 = {
          api = [ nodes.api.networking.primaryIPAddress ];
          worker = [ nodes.worker.networking.primaryIPAddress ];
          dewey = [ nodes.api.networking.primaryIPAddress ];
        };
        networking.nftables.enable = true;
      };
    };
    worker = { nodes, ... }: {
      imports = [
        client
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
      systemd.services.immich-server.wantedBy = lib.mkForce [ ];
    };
  };
  testScript = ''
    import json
    import shlex

    start_all()
    storage.wait_for_unit("nfs-server.service")
    api.wait_for_unit("immich-server.service")
    api.wait_until_succeeds("curl -fsS http://api:2283/api/server/ping")
    worker.fail("systemctl is-active immich-server.service")

    def request(method, path, body=None, token=None):
        command = f"curl -fsS -X {method} http://api:2283/api/{path}"
        if body is not None:
            command += " -H 'Content-Type: application/json' --data " + shlex.quote(json.dumps(body))
        if token:
            command += " -H " + shlex.quote("Authorization: Bearer " + token)
        return json.loads(api.succeed(command))

    with subtest("API cold-starts without a background worker"):
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
        api.succeed("systemctl restart redis-immich.service")
        api.wait_for_unit("redis-immich.service")

    with subtest("Remote worker processes the persisted queue on shared NFS"):
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
