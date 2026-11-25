{ inputs, pkgs }:
let
  images = import ../modules/services/opencloud-images.nix;
  pull =
    image: hash:
    pkgs.dockerTools.pullImage {
      imageName = image.name;
      imageDigest = image.digest;
      finalImageTag = image.tag;
      inherit hash;
    };
  opencloudImage = pull images.opencloud "sha256-1TttoUfFSPjFg+b5rV/CWSOdNJMDaUCaTxnupzzlSRg=";
  officeImage = pull images.office "sha256-HyUBhzYYgV4ffJmXq+c9OmTkw/ssbln3K0Ip+O+Zl48=";
  names = [
    "alpha"
    "beta"
  ];
  ids = {
    alpha = 3101;
    beta = 3102;
  };
  imageUnits = name: {
    "opencloud-${name}" = {
      uid = ids.${name};
      package = opencloudImage;
    };
    "opencloud-${name}-office" = {
      uid = ids.${name};
      package = officeImage;
    };
  };
  instance = quadlet: name: {
    uid = ids.${name};
    gid = ids.${name};
    dataMount = "/mnt/${name}";
    dataSource = "server:/exports/${name}";
    domain = "${name}.example.test";
    listenAddress = if name == "alpha" then "127.0.0.1" else "0.0.0.0";
    ports =
      if name == "alpha" then
        {
          app = 9200;
          office = 9201;
        }
      else
        {
          app = 9300;
          office = 9301;
        };
    image = quadlet.images."opencloud-${name}".ref;
    environmentFile = "/run/opencloud-test/${name}.env";
    oidc.issuer = "https://127.0.0.1:9200";
    # Use OpenCloud's real built-in IDP for this isolated storage test; no
    # Pocket ID, external DNS or production secrets are needed at runtime.
    environment = {
      OC_LOG_LEVEL = "warn";
      COLLABORATION_APP_ADDR = "http://opencloud-${name}-office";
      OC_URL = "https://127.0.0.1:9200";
      PROXY_TLS = "true";
      OC_EXCLUDE_RUN_SERVICES = "";
      OC_INSECURE = "true";
      PROXY_AUTOPROVISION_ACCOUNTS = "false";
    };
    office = {
      domain = "docs-${name}.example.test";
      image = quadlet.images."opencloud-${name}-office".ref;
      environmentFile = "/run/opencloud-test/${name}-office.env";
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "opencloud-rootless-nfs";
  globalTimeout = 900;
  nodes.server = { lib, ... }: {
    services.nfs.server = {
      enable = true;
      exports = lib.concatMapStringsSep "\n" (
        name: "/exports/${name} client(rw,sync,no_subtree_check,root_squash)"
      ) names;
    };
    networking.firewall.allowedTCPPorts = [ 2049 ];
    systemd.tmpfiles.rules = map (
      name: "d /exports/${name} 0700 ${toString ids.${name}} ${toString ids.${name}} - -"
    ) names;
    environment.systemPackages = [ pkgs.attr ];
    system.stateVersion = "26.05";
  };
  nodes.client = { config, lib, ... }: {
    imports = [
      (import ../modules/quadlet-nix.nix inputs)
      ../modules/services/opencloud.nix
    ];
    modules.services.opencloud.instances = lib.genAttrs names (instance config.virtualisation.quadlet);
    virtualisation = {
      memorySize = 12288;
      cores = 4;
      diskSize = 32768;
      quadlet.images = lib.mkMerge (map imageUnits names);
      # Bind test tools, not replacement applications, into the real containers.
      quadlet.containers = lib.genAttrs (map (name: "opencloud-${name}") names) (_: {
        containerConfig.Volume = [ "/nix/store:/nix/store:ro" ];
      });
      fileSystems = lib.genAttrs (map (name: "/mnt/${name}") names) (path: {
        device = "server:/exports/${builtins.baseNameOf path}";
        fsType = "nfs";
        options = [
          "noauto"
          "vers=4.2"
          "noac"
          "hard"
        ];
      });
    };
    networking.firewall.enable = false;
    # Keep test-network addresses until NFS teardown has completed. The legacy
    # scripted test interface removes them before returning NFS delegations.
    networking.useNetworkd = true;
    boot.supportedFilesystems = [ "nfs" ];
    environment.systemPackages = [
      pkgs.attr
      pkgs.jq
    ];
    systemd.services.opencloud-test-credentials = {
      wantedBy = [ "multi-user.target" ];
      before = map (name: "user@${toString ids.${name}}.service") names;
      serviceConfig.Type = "oneshot";
      script = lib.concatMapStringsSep "\n" (name: ''
        install -d -m 0755 /run/opencloud-test
        printf '%s\n' 'IDM_ADMIN_PASSWORD=Test-only-Password-064!' > /run/opencloud-test/${name}.env
        printf '%s\n' 'JWT_SECRET=test-only-office-${name}-064' > /run/opencloud-test/${name}-office.env
        chown ${toString ids.${name}}:${toString ids.${name}} /run/opencloud-test/${name}*.env
        chmod 0400 /run/opencloud-test/${name}*.env
      '') names;
    };
    system.stateVersion = "26.05";
  };
  testScript = ''
    import json

    def podman(name, args):
        return client.succeed(f"sudo -u opencloud-{name} -- podman {args}")

    def ready(name):
        port = 9200 if name == "alpha" else 9300
        client.wait_until_succeeds(f"curl -kfsS https://127.0.0.1:{port}/status.php", timeout=300)
    def office_ready(name):
        port = 9201 if name == "alpha" else 9301
        client.wait_until_succeeds(f"curl -fsS http://127.0.0.1:{port}/hosting/discovery", timeout=300)


    client.start()
    client.wait_for_unit("multi-user.target")
    for name in ["alpha", "beta"]:
        client.wait_for_unit(f"user@{3101 if name == 'alpha' else 3102}.service")
        # The first boot loads both images; only then does the application get
        # past its office preflight to the NFS check this asserts on.
        office_ready(name)
        client.wait_until_succeeds(f"test $(systemctl --user --machine=opencloud-{name}@ show opencloud-{name}.service -p NRestarts --value) -ge 1", timeout=120)
        client.succeed(f"test ! -e /mnt/{name}/users; test ! -e /mnt/{name}/metadata")
        assert json.loads(podman(name, "ps --format json")) == [] or all(
            c["Names"][0].endswith("-office") for c in json.loads(podman(name, "ps --format json"))
        )

    server.start()
    server.wait_for_unit("nfs-server.service")
    for name in ["alpha", "beta"]:
        client.succeed(f"systemctl start mnt-{name}.mount")
        mount_options = set(client.succeed(f"findmnt -n -o OPTIONS /mnt/{name}").strip().split(","))
        assert {"vers=4.2", "noac", "hard"} <= mount_options
        ready(name)
        assert podman(name, f"exec opencloud-{name} id -u").strip() == "1000"
        assert podman(name, "info --format '{{.Store.GraphRoot}}'").strip().startswith(f"/var/lib/opencloud-{name}/")
        podman(name, f"exec opencloud-{name} sh -c 'echo persistent > /data/probe'")
        # Execute xattr operations inside the application's keep-id namespace.
        podman(name, f"exec opencloud-{name} ${pkgs.attr}/bin/setfattr -n user.opencloud-test -v {name} /data/probe")
        server.succeed(f"test $(stat -c %u /exports/{name}/probe) = {3101 if name == 'alpha' else 3102}")
        server.succeed(f"getfattr --only-values -n user.opencloud-test /exports/{name}/probe | grep -Fx {name}")
        podman(name, f"exec opencloud-{name} sh -c 'echo durable > /var/lib/opencloud/probe'")
        office_ready(name)
        podman(name, f"exec opencloud-{name}-office sh -c 'echo office-durable > /var/www/euro-office/Data/probe'")
        client.fail(f"sudo -u opencloud-{'beta' if name == 'alpha' else 'alpha'} -- cat /mnt/{name}/probe")
        client.fail(f"sudo -u opencloud-{'beta' if name == 'alpha' else 'alpha'} -- cat /var/lib/opencloud-{name}/data/probe")
        client.fail(f"sudo -u opencloud-{'beta' if name == 'alpha' else 'alpha'} -- sh -c 'echo forbidden >> /mnt/{name}/probe'")
        client.fail(f"sudo -u opencloud-{'beta' if name == 'alpha' else 'alpha'} -- sh -c 'echo forbidden >> /var/lib/opencloud-{name}/data/probe'")
        client.fail(f"sudo -u opencloud-{'beta' if name == 'alpha' else 'alpha'} -- cat /run/opencloud-test/{name}.env")
        container_id = podman(name, f"inspect --format '{{{{.Id}}}}' opencloud-{name}").strip()
        client.succeed(f"systemctl --user --machine=opencloud-{name}@ restart opencloud-{name}.service")
        ready(name)
        assert podman(name, f"inspect --format '{{{{.Id}}}}' opencloud-{name}").strip() != container_id
        podman(name, f"exec opencloud-{name} sh -c 'grep -Fx persistent /data/probe; grep -Fx durable /var/lib/opencloud/probe'")

    server.succeed("curl -kfsS https://client:9300/status.php")
    server.fail("curl --connect-timeout 3 -kfsS https://client:9200/status.php")
    assert json.loads(client.succeed("podman ps --format json")) == []
    client.shutdown()
    client.start()
    client.wait_for_unit("multi-user.target")
    for name in ["alpha", "beta"]:
        client.succeed(f"systemctl start mnt-{name}.mount")
        ready(name)
        podman(name, f"exec opencloud-{name} sh -c 'grep -Fx persistent /data/probe; grep -Fx durable /var/lib/opencloud/probe'")
        client.succeed(f"sudo -u opencloud-{name} -- getfattr --only-values -n user.opencloud-test /mnt/{name}/probe | grep -Fx {name}")
        office_ready(name)
        podman(name, f"exec opencloud-{name}-office grep -Fx office-durable /var/www/euro-office/Data/probe")
  '';
}
