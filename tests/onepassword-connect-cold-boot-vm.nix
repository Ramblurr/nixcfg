{ inputs, pkgs }:
let
  eval = import ./onepassword-connect-eval.nix { inherit inputs pkgs; };
  inherit (eval) user;
  pod = (eval.evaluate "debord").virtualisation.quadlet.pods.op-connect;
  port = toString (import ../configs/home-ops/onepassword-connect-tls.nix).port;
  siteLib =
    (import inputs.nixpkgs {
      inherit (pkgs.stdenv.hostPlatform) system;
      overlays = [ (import ../lib inputs) ];
    }).lib;
  stubImage = pkgs.dockerTools.buildImage {
    name = "connect-stub";
    tag = "latest";
    copyToRoot = [ pkgs.busybox ];
    extraCommands = ''
      mkdir -p www/v1
      echo ok > www/heartbeat
      echo ok > www/v1/vaults
    '';
    config.Cmd = [
      "httpd"
      "-f"
      "-vv"
      "-p"
      port
      "-h"
      "/www"
    ];
  };
  nic = name: {
    netdevConfig = {
      Name = name;
      Kind = "veth";
    };
    peerConfig.Name = "${name}-peer";
  };
  peer = name: activation: {
    matchConfig.Name = "${name}-peer";
    linkConfig = {
      ActivationPolicy = activation;
      RequiredForOnline = false;
    };
    networkConfig.LinkLocalAddressing = false;
  };
in
pkgs.testers.runNixOSTest {
  name = "onepassword-connect-cold-boot";
  node.specialArgs.lib = siteLib;

  nodes.debord =
    { lib, ... }:
    {
      imports = [
        (import ../modules/quadlet-nix.nix inputs)
        ../config/site.nix
        ../modules/site-net
      ];
      options.repo = {
        secrets = lib.mkOption { type = lib.types.attrs; };
        secretFiles = lib.mkOption { type = lib.types.attrs; };
      };
      config = {
        repo.secrets.global = {
          domain.home = "example.test";
          email.home = "ops@example.test";
          pubKeys = [ ];
        };

        virtualisation.memorySize = 2048;
        virtualisation.vlans = [ ];
        networking.interfaces = lib.mkForce { };
        networking.nftables.enable = true;
        services.timesyncd.enable = lib.mkForce false;

        systemd.network = {
          netdevs = {
            "10-lan0" = nic "lan0";
            "10-lan1" = nic "lan1";
          };
          networks."10-lan1" = {
            matchConfig.Name = "lan1";
            networkConfig = {
              VLAN = [ "vlan-data" ];
              LinkLocalAddressing = false;
            };
            linkConfig = {
              MTUBytes = 9000;
              RequiredForOnline = "carrier";
            };
          };
          networks."10-lan0-peer" = peer "lan0" "manual";
          networks."10-lan1-peer" = peer "lan1" "up";
        };

        environment.systemPackages = [ pkgs.curl ];

        users.groups.${user.name}.gid = user.uid;
        users.users.${user.name} = {
          inherit (user) uid;
          isNormalUser = true;
          group = user.name;
          linger = true;
          autoSubUidGidRange = true;
        };

        virtualisation.quadlet = {
          enable = true;
          pods.op-connect = {
            inherit (user) uid;
            inherit (pod) podConfig serviceConfig unitConfig;
            autoStart = true;
          };
          containers.op-connect-api = {
            inherit (user) uid;
            autoStart = true;
            containerConfig = {
              Image = "docker-archive:${stubImage}";
              Pod = "op-connect.pod";
            };
          };
        };
      };
    };

  testScript =
    { nodes, ... }:
    let
      mgmt = nodes.debord.site.net.mgmt;
      router = builtins.head mgmt.hosts4.${mgmt.dhcp.router};
      localAddress = builtins.head mgmt.hosts4.debord;
      virtualAddress = builtins.head mgmt.hosts4.onepassword-connect;
      uid = toString user.uid;
    in
    ''
      def as_user(command):
          return debord.succeed(
              f"cd / && runuser -u ${user.name} -- env XDG_RUNTIME_DIR=/run/user/${uid} {command}"
          ).strip()

      def identity():
          containers = as_user("podman pod inspect op-connect --format '{{range .Containers}}{{.Id}} {{end}}'")
          return {
              "pod": as_user("podman pod inspect op-connect --format '{{.ID}} {{.InfraContainerID}}'"),
              "containers": as_user(
                  f"podman inspect {containers} --format '{{{{.Name}}}} {{{{.State.StartedAt}}}} {{{{.RestartCount}}}}'"
              ),
              "units": as_user(
                  "systemctl --user show op-connect-pod.service op-connect-api.service -p InvocationID -p NRestarts"
              ),
          }

      def pod_routes(ident):
          infra = ident["pod"].split()[1]
          pid = as_user(f"podman inspect {infra} --format '{{{{.State.Pid}}}}'")
          return debord.succeed(f"nsenter --target {pid} --net ip -4 addr show; nsenter --target {pid} --net ip -4 route show")

      debord.start()
      debord.wait_for_unit("user@${uid}.service")
      debord.wait_for_unit("op-connect-api.service", user="${user.name}", timeout=300)
      debord.succeed("ip link show lan0 | grep -q NO-CARRIER")
      debord.wait_until_succeeds("curl --fail --silent --max-time 2 http://127.0.0.1:${port}/heartbeat", timeout=60)
      before = identity()
      print(before)
      assert len(before["containers"].splitlines()) == 2, before
      debord.succeed("curl --fail --silent --show-error --max-time 5 http://127.0.0.1:${port}/heartbeat")
      print(debord.succeed("ip -4 addr show; ip -4 route show; ip -4 route show table all | grep -v '^local\\|^broadcast'"))
      print(pod_routes(before))

      debord.succeed("ip link set lan0-peer up")
      debord.wait_until_succeeds("ip -4 route show default | grep 'via ${router} dev mgmt' | grep -vq linkdown", timeout=30)
      debord.succeed("ip addr add ${virtualAddress}/32 dev mgmt")

      debord.succeed("curl --fail --silent --show-error --max-time 10 http://${virtualAddress}:${port}/v1/vaults")

      routes = pod_routes(before)
      print(routes)
      assert "inet ${localAddress}/${toString mgmt.subnet4Len}" in routes, routes
      assert "default via ${router} dev mgmt" in routes, routes
      after = identity()
      assert after == before, (before, after)
      assert after["units"].count("NRestarts=0") == 2, after
    '';
}
