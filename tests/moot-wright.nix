{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  mkGuest =
    extra:
    inputs.self.lib.nixcfg.mkGuest "workstead" {
      hostPath = ./fixtures/moot-wright;
      extraModules = [ extra ];
    };
  guest = mkGuest { };
  cfg = guest.config;
  builderCfg =
    (mkGuest {
      modules.microvm-guest.mootWright.nix.remoteBuilder = {
        hostName = "moot-builder.example.ts.net";
        publicHostKey = sshKeys.snakeOilEd25519PublicKey;
      };
    }).config;
  wright = cfg.modules.microvm-guest.mootWright;
  inherit (wright) name;
  inherit (cfg.modules.microvm-guest.mootVm) network zvolSize;
  netns = "moot-${name}";
  service = n: cfg.systemd.services.${n};
  gitConfig = lib.foldl' lib.recursiveUpdate { } cfg.programs.git.config;
  record = {
    tap = {
      id = network.tapId;
      inherit (network) hostAddress guestAddress;
    };
    uplink = {
      id = "mn-${name}";
      hostAddress = "172.31.255.1";
      nodeAddress = "172.31.255.2";
    };
    endpoint = {
      address = "192.168.1.1";
      port = 41642;
      allowFrom = [ "192.168.1.2" ];
    };
    advertiseTags = [ "tag:moot-wright" ];
    authKeyFile = "/run/keys/${name}-ts-authkey";
    inherit (network) dnsResolver;
  };
  mkBareHost =
    extra:
    (inputs.nixpkgs.lib.nixosSystem {
      inherit (pkgs.stdenv.hostPlatform) system;
      specialArgs = { inherit inputs; };
      modules = [
        inputs.microvm.nixosModules.host
        inputs.impermanence.nixosModules.impermanence
        ../modules/microvm-host
        {
          modules.microvm-host = {
            enable = true;
            baseZfsDataset = "rpool/test";
          };
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
        }
        extra
      ];
    }).config;
  bareHost = mkBareHost { };
  failures =
    scope: c:
    lib.filter (lib.hasInfix scope) (map (a: a.message) (lib.filter (a: !a.assertion) c.assertions));
  nodeHost =
    {
      nodes ? {
        ${name} = record;
      },
      vms ? {
        ${name} = {
          evaluatedConfig = guest;
          autostart = false;
        };
      },
    }:
    mkBareHost {
      modules.microvm-host = {
        mootNodeUplinks = [ "eth1" ];
        mootNodes = nodes;
      };
      microvm.vms = vms;
    };
  nodeFailures = args: failures "mootNodes" (nodeHost args);
  host = nodeHost { };
  hostUnit = n: host.systemd.services.${n};
  envFile = c: c.environment.etc."moot-nodes/${name}.env".text;
  other = lib.recursiveUpdate record {
    uplink = {
      id = "mn-other";
      hostAddress = "172.31.255.5";
      nodeAddress = "172.31.255.6";
    };
    endpoint.port = 41644;
  };
in
assert lib.assertMsg
  (
    cfg.microvm.hypervisor == "cloud-hypervisor"
    && cfg.microvm.storeOnDisk
    && cfg.modules.microvm-guest.zvols.nix-rw.size == zvolSize
    && cfg.microvm.vsock.ssh.enable
    && cfg.microvm.credentialFiles == { }
    && !cfg.modules.microvm-guest.bootstrapSops.enable
  )
  "the wright runs under cloud-hypervisor on a blk store with a zvol overlay, VSOCK SSH and no SOPS bootstrap";
assert lib.assertMsg (
  map (i: { inherit (i) type id; }) cfg.microvm.interfaces == [
    {
      type = "tap";
      id = network.tapId;
    }
  ]
  && !cfg.modules.microvm-guest.autoNetSetup.enable
) "the wright's only interface is its tap";
assert lib.assertMsg (
  !cfg.systemd.sockets.sshd.enable
  && !(cfg.systemd.services ? sshd)
  && !cfg.services.openssh.openFirewall
  &&
    cfg.services.openssh.hostKeys == [
      {
        path = "/var/lib/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ]
) "sshd listens only on VSOCK, with a persistent host key";
assert lib.assertMsg (
  !cfg.modules.microvm-guest.hostSecrets.enable
  && !(lib.any (s: s.tag == "host-secrets") cfg.microvm.shares)
  && !(cfg.systemd.services ? moot-wright-credentials)
) "without a remote builder the guest gets no host secrets";
assert lib.assertMsg (
  !builderCfg.modules.microvm-guest.hostSecrets.enable
  && lib.all (m: m.sshKey == null) builderCfg.nix.buildMachines
) "with a remote builder the guest still holds no SSH key";
assert lib.assertMsg (
  !(cfg.systemd.services ? "ts-plug-${name}")
  && !(cfg.users.users ? "ts-${name}")
  && !(cfg.systemd.services ? "systemd-netns-private-${netns}")
  && lib.all (n: !((service n).serviceConfig ? NetworkNamespacePath)) [
    "t3-${name}"
    "nix-daemon"
  ]
) "the guest runs no tailnet node and no network namespace";
assert lib.assertMsg (
  cfg.nix.settings.max-jobs == 1
  && cfg.nix.settings.extra-substituters == [ "https://moot-cache.example.ts.net" ]
  && cfg.nix.buildMachines == [ ]
  && !cfg.nix.distributedBuilds
) "without a remote builder the guest builds locally, with the moot cache as substituter";
assert lib.assertMsg (
  builderCfg.nix.settings.max-jobs == 0
  && map (m: m.protocol) builderCfg.nix.buildMachines == [ "ssh-ng" ]
  && builderCfg.nix.distributedBuilds
) "with a remote builder, builds go only to it over ssh-ng";
assert lib.assertMsg (
  gitConfig.user.email == "${name}@moot.land"
  && gitConfig.user.name == name
  && wright.identity.matrixId == "@${name}:moot.land"
) "the wright's identity defaults to its name";
assert lib.assertMsg (lib.any (lib.hasInfix "distinct host addresses") (
  failures "mootVm"
    (mkGuest {
      modules.microvm-guest.mootVm.network.guestAddress = lib.mkForce "10.77.0.5";
    }).config
)) "the guest's tap addresses share one subnet";
assert lib.assertMsg (
  nodeFailures { } == [ ]
  &&
    nodeFailures {
      nodes = {
        ${name} = record;
        inherit other;
      };
    } == [ ]
) "a node record that agrees with its guest passes";
assert lib.assertMsg (
  lib.any (lib.hasInfix "disagrees with its guest on t3Port") (nodeFailures {
    nodes.${name} = record // {
      t3Port = 3781;
    };
  })
  && lib.any (lib.hasInfix "disagrees with its guest on tap.guestAddress") (nodeFailures {
    nodes.${name} = lib.recursiveUpdate record {
      tap.guestAddress = "10.77.0.1";
      tap.hostAddress = "10.77.0.2";
    };
  })
  && lib.any (lib.hasInfix "${name} has no modules.microvm-host.mootNodes record") (nodeFailures {
    nodes = { };
  })
) "the host checks every wright VM against its node record";
assert lib.assertMsg
  (
    lib.any (lib.hasInfix "share uplink ids: mn-${name}") (nodeFailures {
      vms = { };
      nodes = {
        a = record;
        b = lib.recursiveUpdate other { uplink.id = "mn-${name}"; };
      };
    })
    && lib.any (lib.hasInfix "overlapping uplink subnets: a/b") (nodeFailures {
      vms = { };
      nodes = {
        a = record;
        b = lib.recursiveUpdate other { uplink.prefixLength = 28; };
      };
    })
    && lib.any (lib.hasInfix "share endpoints: 192.168.1.1:41642") (nodeFailures {
      vms = { };
      nodes = {
        a = record;
        b = lib.recursiveUpdate other { endpoint.port = 41642; };
      };
    })
    && lib.any (lib.hasInfix "uplink overlaps its tap") (nodeFailures {
      vms = { };
      nodes.a = lib.recursiveUpdate record {
        uplink = {
          hostAddress = "10.77.0.1";
          nodeAddress = "10.77.0.2";
        };
      };
    })
  )
  "node uplinks need unique ids and disjoint subnets, apart from their taps, and nodes unique endpoints";
assert lib.assertMsg (lib.all
  (
    u:
    u.serviceConfig.NetworkNamespacePath == "/run/netns/${netns}"
    && builtins.elem "moot-node-netns@${name}.service" u.bindsTo
    && builtins.elem "moot-node-netns@${name}.service" u.after
    && u.overrideStrategy == "asDropin"
  )
  [
    (hostUnit "microvm@${name}")
    (hostUnit "microvm-tap-interfaces@${name}")
  ]
) "the VM and its tap live in the node's namespace";
assert lib.assertMsg (
  builtins.elem "nftables.service" (hostUnit "moot-node-netns@").bindsTo
  && builtins.elem "moot-node@${name}.service" (hostUnit "microvm@${name}").wants
) "the node's namespace stops with the host firewall, and the VM pulls in its node";
assert lib.assertMsg (
  lib.hasInfix "MOOT_ADVERTISE_TAGS=tag:moot-wright\n" (envFile host)
  && lib.hasInfix "MOOT_ADVERTISE_TAGS=\n" (
    envFile (nodeHost {
      nodes.${name} = record // {
        advertiseTags = [ ];
      };
    })
  )
) "a pre-tagged auth key starts the node without tags";
assert lib.assertMsg (
  !bareHost.networking.nftables.enable
  && !(bareHost.networking.nftables.tables ? moot-nodes)
  && !(bareHost.boot.kernel.sysctl ? "net.ipv4.conf.all.forwarding")
) "a host without node records keeps its firewall and sysctls";
pkgs.testers.runNixOSTest {
  name = "moot-wright";
  node.specialArgs = { inherit inputs; };

  nodes.lan = {
    networking.firewall.enable = false;
    networking.interfaces.eth1.ipv4 = {
      addresses = [
        {
          address = "198.51.100.10";
          prefixLength = 24;
        }
      ];
      routes = [
        {
          address = "10.77.0.0";
          prefixLength = 30;
          via = "192.168.1.1";
        }
      ];
    };
    systemd.services.peer-echo = {
      wantedBy = [ "multi-user.target" ];
      script = ''exec ${lib.getExe pkgs.socat} TCP-LISTEN:80,fork,reuseaddr SYSTEM:'echo "peer=$SOCAT_PEERADDR"' '';
    };
    systemd.services.udp-sink = {
      wantedBy = [ "multi-user.target" ];
      script = "exec ${lib.getExe pkgs.socat} -u UDP-RECV:9999,reuseaddr OPEN:/run/udp-sink,creat,append";
    };
    systemd.services.wg-sink = {
      wantedBy = [ "multi-user.target" ];
      script = "exec ${lib.getExe pkgs.socat} -u UDP-RECV:41641,reuseaddr OPEN:/run/wg-sink,creat,append";
    };
    systemd.services.other-port-sink = {
      wantedBy = [ "multi-user.target" ];
      script = "exec ${lib.getExe pkgs.socat} -u UDP-RECV:41643,reuseaddr OPEN:/run/wg-sink,creat,append";
    };
    systemd.services.dns-sink = {
      wantedBy = [ "multi-user.target" ];
      script = "exec ${lib.getExe pkgs.socat} -u UDP-RECV:53,reuseaddr OPEN:/run/dns-sink,creat,append";
    };
    environment.systemPackages = [ pkgs.socat ];
  };

  nodes.host = {
    imports = [
      inputs.microvm.nixosModules.host
      inputs.impermanence.nixosModules.impermanence
      ../modules/microvm-host
    ];
    modules.microvm-host = {
      enable = true;
      baseZfsDataset = "tank/microvms";
      mootNodeUplinks = [ "eth1" ];
      mootNodePeers = [
        {
          address = "192.168.1.2";
          port = 41641;
        }
      ];
      mootNodes.${name} = record;
    };
    microvm.vms.${name} = {
      evaluatedConfig = guest;
      autostart = false;
    };
    networking.interfaces.eth1.ipv4.routes = [
      {
        address = "198.51.100.0";
        prefixLength = 24;
        via = "192.168.1.2";
      }
    ];
    boot.supportedFilesystems = [ "zfs" ];
    networking.hostId = "8425e349";
    networking.useNetworkd = true;
    systemd.services.host-echo = {
      wantedBy = [ "multi-user.target" ];
      script = "exec ${lib.getExe pkgs.socat} TCP-LISTEN:8080,fork,reuseaddr SYSTEM:'echo host'";
    };
    environment.systemPackages = [
      pkgs.socat
      pkgs.curl
      pkgs.tailscale
    ];
    virtualisation = {
      cores = 4;
      memorySize = 5120;
      emptyDiskImages = [ 8192 ];
      qemu.options = [
        "-cpu"
        "host"
      ];
      additionalPaths = [ cfg.microvm.deploy.sshSwitch ];
    };
    environment.etc."microvm-test-key" = {
      source = sshKeys.snakeOilEd25519PrivateKey;
      mode = "0600";
    };
    programs.ssh.extraConfig = ''
      Host vsock-mux/*
        User root
        IdentityFile /etc/microvm-test-key
        BatchMode yes
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
    '';
  };

  testScript = ''
    import base64
    import json
    import shlex

    name = "${name}"
    uid = 2001
    netns = "${netns}"
    state = f"/var/lib/microvms/{name}"
    target = f"root@vsock-mux/{state}/notify.vsock"
    ssh = f"ssh -n {target}"
    authkey = "tskey-auth-moot-wright-test-0b5e"
    home = f"/home/{name}"
    tap = "${network.tapId}"
    tap_host = "${network.hostAddress}"
    guest_addr = "${network.guestAddress}"
    spoofed = "192.168.1.77"
    node = f"moot-node@{name}.service"
    peer = "192.168.1.2"
    node_addr = "${record.uplink.nodeAddress}"
    endpoint = "${record.endpoint.address}:${toString record.endpoint.port}"
    localapi = f"/var/lib/private/moot-nodes/{name}/localapi.sock"
    in_node = f"ip netns exec {netns}"
    as_wright = f"setpriv --reuid={uid} --regid={uid} --init-groups"
    targets = {
        "internet": ("198.51.100.10", 80),
        "lan": ("192.168.1.2", 80),
        "host on the lan": ("192.168.1.1", 8080),
        "node tap address": (tap_host, 8080),
        "tailnet": ("100.100.100.100", 80),
    }


    def guest(cmd):
        return host.succeed(f"{ssh} {shlex.quote(cmd)}")


    def guest_fail(cmd):
        host.fail(f"{ssh} {shlex.quote(cmd)}")


    def wright(cmd):
        return guest(f"runuser -l {name} -c {shlex.quote(cmd)}")


    def tcp(addr, port, opts=""):
        return f"socat -T3 - TCP:{addr}:{port},connect-timeout=3{opts} </dev/null"


    def udp(tag, addr, port, opts=""):
        return f"echo {tag} | socat -u - UDP:{addr}:{port}{opts}"


    def write_json(path, value):
        data = base64.b64encode(json.dumps(value).encode()).decode()
        wright(f"echo {data} | base64 -d > {path}")


    def main_pid(unit):
        return guest(f"systemctl show -P MainPID {unit}").strip()


    def host_pid(unit):
        return host.succeed(f"systemctl show -P MainPID {unit}").strip()


    def node_drops():
        rules = json.loads(host.succeed(f"{in_node} nft -j list chain inet moot-node prerouting"))["nftables"]
        exprs = [r["rule"]["expr"] for r in rules if "rule" in r]
        return sum(e["counter"]["packets"] for expr in exprs if any("drop" in e for e in expr) for e in expr if "counter" in e)


    def peer_accepts():
        rules = json.loads(host.succeed("nft -j list chain inet moot-nodes forward"))["nftables"]
        exprs = [r["rule"]["expr"] for r in rules if "rule" in r and "@moot-peers" in json.dumps(r)]
        assert exprs, "no rule matches @moot-peers"
        return sum(e["counter"]["packets"] for expr in exprs for e in expr if "counter" in e)


    lan.start()
    host.start()
    lan.wait_for_unit("peer-echo.service")
    lan.wait_for_unit("udp-sink.service")
    lan.wait_for_unit("dns-sink.service")
    lan.wait_for_unit("wg-sink.service")
    lan.wait_for_unit("other-port-sink.service")
    host.wait_for_unit("multi-user.target")
    # As on the real hosts: virtiofs checks a non-root guest user against POSIX ACLs.
    host.succeed("zpool create -O mountpoint=none -O acltype=posixacl -O xattr=sa tank /dev/vdb")
    host.succeed(
        "install -d -m 0700 /run/keys"
        f" && printf %s {authkey} > /run/keys/{name}-ts-authkey"
        f" && chmod 0400 /run/keys/{name}-ts-authkey"
    )
    host.succeed(f"systemctl start microvm@{name}.service")
    host.wait_until_succeeds(f"{ssh} true", timeout=600)
    host.wait_for_unit(node)
    guest("systemctl is-system-running --wait || true")
    boot_id = guest("cat /proc/sys/kernel/random/boot_id").strip()

    with subtest("the VM's tap and its node live only in the node's namespace"):
        host.fail(f"ip link show {tap}")
        host.succeed(f"{in_node} ip -4 addr show {tap} | grep -q '{tap_host}/30'")
        for unit in [f"microvm@{name}.service", node]:
            assert host.succeed(f"ip netns identify {host_pid(unit)}").strip() == netns, unit
        guest(f"ip -4 addr show uplink | grep -q '{guest_addr}/30'")
        guest("! ss -Htln | grep -q ':22 '")
        guest("test -s /var/lib/ssh/ssh_host_ed25519_key")
        guest("findmnt -n -o FSTYPE /nix/.rw-store | grep -qx ext4")

    with subtest("the guest holds no tailnet key and no node state"):
        guest("! pgrep -x ts-plug")
        guest(f"! getent passwd ts-{name}")
        guest("! ls /var/lib | grep -q ts-plug")
        guest("! test -e /run/host-secrets/ts-authkey")
        # grep exits 1 only when it read everything and matched nothing.
        guest(f"rc=0; grep -rlFs -D skip -e {authkey} /etc /var /home /run || rc=$?; test $rc -eq 1")
        environ = host.succeed(f"tr '\\0' '\\n' < /proc/{host_pid(node)}/environ")
        assert f"TS_AUTHKEY={authkey}" in environ.splitlines(), "the node did not get its auth key"
        cmdline = host.succeed(f"tr '\\0' ' ' < /proc/{host_pid(node)}/cmdline")
        for want in [
            f"-hostname {name} ",
            f"-https-port 443:{guest_addr}:3780 ",
            f"-http-proxy {tap_host}:1056 ",
            "-exit-node ward ",
            "-dns-resolver 198.51.100.10:53 ",
            "-transparent-tcp 127.0.0.1:15001 ",
            "-transparent-udp 127.0.0.1:15001 ",
            "-port 41642 ",
            f"-static-endpoint {endpoint} ",
            f"-localapi-socket /var/lib/moot-nodes/{name}/localapi.sock ",
            "-advertise-tags tag:moot-wright",
        ]:
            assert want in cmdline, f"{want}: {cmdline}"
        host.succeed(f"test -d /var/lib/private/moot-nodes/{name}")

    with subtest("the node listens on its fixed port and answers on its LocalAPI socket"):
        host.wait_until_succeeds(f"{in_node} ss -Hulnp | grep ':41642 ' | grep -q ts-plug")
        host.wait_until_succeeds(f"test -S {localapi}")
        status = json.loads(host.succeed(f"tailscale --socket {localapi} status --json"))
        assert "BackendState" in status, status

    with subtest("T3 answers the node's inbound path and nothing else"):
        guest(f"systemctl start moot-wright-config.service t3-{name}.service")
        guest(f"systemctl is-active t3-{name}.service user@{uid}.service")
        # The node's -https-port 443:<guest>:3780 reverse proxy dials this
        # address from the node's namespace. Without a control server the
        # node cannot log in, so a client in that namespace stands in for it.
        host.wait_until_succeeds(
            f"{in_node} curl -s -o /dev/null -w '%{{http_code}}' http://{guest_addr}:3780/ | grep -qv '^000$'",
            timeout=300,
        )
        guest(f"ss -Htln | grep -q '{guest_addr}:3780'")
        host.fail(f"curl -s --connect-timeout 3 http://{guest_addr}:3780/")
        lan.fail(f"socat -T3 - TCP:{guest_addr}:3780,connect-timeout=3 </dev/null")

    with subtest("nix-daemon builds locally and fetches through the node's proxy"):
        guest("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        wright("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        environ = guest(f"tr '\\0' '\\n' < /proc/{main_pid('nix-daemon')}/environ")
        assert f"HTTPS_PROXY=http://{tap_host}:1056" in environ, environ
        conf = guest("cat /etc/nix/nix.conf")
        assert "max-jobs = 1" in conf, conf
        assert "extra-substituters = https://moot-cache.example.ts.net" in conf, conf
        guest("! test -s /etc/nix/machines")
        drv = 'derivation { name = "moot-local"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo built > $out" ]; }'
        out = guest(f"NIX_REMOTE=daemon nix-build --no-out-link --option substitute false -E {shlex.quote(drv)}").strip()
        assert guest(f"cat {out}").strip() == "built"

    with subtest("with the node stopped, nothing leaves the VM"):
        host.succeed(f"systemctl stop {node}")
        host.succeed(f"systemctl is-active microvm@{name}.service moot-node-netns@{name}.service")
        guest(f"ip addr add {spoofed}/32 dev uplink")
        guest("sysctl -qw net.ipv6.conf.uplink.disable_ipv6=0 && ip -6 addr add fd00::2/64 dev uplink nodad && ip -6 route add default dev uplink")
        before = node_drops()
        for addr, port in targets.values():
            guest_fail(tcp(addr, port))
            guest_fail(f"{as_wright} {tcp(addr, port)}")
            guest_fail(tcp(addr, port, f",bind={spoofed}"))
        for addr in ["198.51.100.10", "192.168.1.1", tap_host]:
            guest_fail(f"ping -c1 -W2 {addr}")
        guest_fail("socat -T3 - 'TCP6:[2001:db8::10]:80,connect-timeout=3' </dev/null")
        guest(udp("leak-root", "198.51.100.10", 9999))
        guest(f"{as_wright} {udp('leak-wright', '198.51.100.10', 9999)}")
        guest(udp("leak-spoofed", "198.51.100.10", 9999, f",bind={spoofed}"))
        guest(udp("leak-peer", peer, 41641))
        guest(f"{as_wright} {udp('leak-peer-wright', peer, 41641)}")
        guest(udp("leak-peer-spoofed", peer, 41641, f",bind={spoofed}"))
        guest("echo leak-v6 | socat -u - 'UDP6:[2001:db8::10]:9999' || true")
        guest("timeout 15 getent hosts leak-dns.example || true")
        lan.succeed("sleep 3; ! grep -aqs leak /run/udp-sink /run/dns-sink /run/wg-sink")
        assert node_drops() > before, "the node's namespace dropped nothing"

    with subtest("the node's own WireGuard reaches only listed LAN peers, and they reach it"):
        before = peer_accepts()
        host.succeed(f"echo node-wg | {in_node} socat -u - UDP:{peer}:41641,sourceport=41642")
        host.succeed(f"echo node-other-port | {in_node} socat -u - UDP:{peer}:41643")
        host.fail(f"{in_node} {tcp(peer, 80)}")
        lan.wait_until_succeeds("grep -aq node-wg /run/wg-sink", timeout=30)
        lan.succeed("sleep 2; ! grep -aq node-other-port /run/wg-sink")
        assert peer_accepts() > before, "the peer rule accepted nothing"
        host.succeed(f"systemd-run --unit=node-wg {in_node} ${lib.getExe pkgs.socat} UDP-LISTEN:41642,bind={node_addr},reuseaddr,fork SYSTEM:'echo node'")
        host.succeed("sleep 1")
        assert lan.succeed(f"echo peer | socat -T3 - UDP:{endpoint}").strip() == "node"
        lan.succeed("ip addr add 192.168.1.50/24 dev eth1")
        _, stranger = lan.execute(f"echo stranger | socat -T3 - UDP:{endpoint},bind=192.168.1.50")
        assert stranger.strip() != "node", "an unlisted LAN host reached the node"
        lan.succeed("ip addr del 192.168.1.50/24 dev eth1")
        host.succeed("systemctl stop node-wg")

    with subtest("every guest flow arrives at the node's capture and nowhere else"):
        # The node cannot log in without a control server, so it never opens
        # its listeners; probes on the same addresses stand in for them.
        host.succeed(f"systemd-run --unit=capture-tcp {in_node} ${lib.getExe pkgs.socat} TCP-LISTEN:15001,bind=127.0.0.1,ip-transparent,reuseaddr,fork SYSTEM:'echo captured'")
        host.succeed(f"systemd-run --unit=capture-udp {in_node} ${lib.getExe pkgs.socat} -u UDP-RECV:15001,bind=127.0.0.1,ip-transparent,reuseaddr OPEN:/run/captured-udp,creat,append")
        host.succeed(f"systemd-run --unit=proxy-probe {in_node} ${lib.getExe pkgs.socat} TCP-LISTEN:1056,bind={tap_host},reuseaddr,fork SYSTEM:'echo proxy'")
        host.succeed("sleep 1")
        for label, (addr, port) in targets.items():
            assert guest(tcp(addr, port)).strip() == "captured", label
            guest_fail(tcp(addr, port, f",bind={spoofed}"))
        assert guest(tcp(tap_host, 1056)).strip() == "proxy"
        guest(udp("captured-udp", "198.51.100.10", 9999))
        guest(udp("spoofed-udp", "198.51.100.10", 9999, f",bind={spoofed}"))
        guest(udp("captured-peer", peer, 41641))
        guest("timeout 5 getent hosts captured-dns.example || true")
        host.wait_until_succeeds("grep -aq captured-udp /run/captured-udp && grep -aq captured-dns /run/captured-udp && grep -aq captured-peer /run/captured-udp")
        host.succeed("sleep 2; ! grep -aq spoofed /run/captured-udp")
        lan.succeed("! grep -aqs -e leak -e captured -e spoofed /run/udp-sink /run/dns-sink /run/wg-sink")
        host.succeed("systemctl stop capture-tcp capture-udp proxy-probe")
        guest(f"ip addr del {spoofed}/32 dev uplink && ip -6 addr del fd00::2/64 dev uplink")
        host.succeed(f"systemctl start {node}")

    with subtest("nothing outside the node reaches into the guest"):
        guest("systemd-run --unit=guest-echo socat TCP-LISTEN:7000,fork,reuseaddr SYSTEM:'echo guest'")
        guest("sleep 1; socat -T3 - TCP:127.0.0.1:7000 </dev/null | grep -qx guest")
        lan.fail(f"socat -T3 - TCP:{guest_addr}:7000,connect-timeout=3 </dev/null")
        host.fail(f"socat -T3 - TCP:{guest_addr}:7000,connect-timeout=3 </dev/null")

    with subtest("the wright cannot notify the host's microvm unit over VSOCK"):
        for prefix in [as_wright, ""]:
            guest(f"printf STATUS=leak | timeout 5 {prefix} socat -u - VSOCK-CONNECT:2:8888 || true")
        host.succeed("sleep 2")
        host.fail(f"test -e {state}/notify.vsock_8888")
        host.fail("ss -Hxl | grep -F notify.vsock_8888")
        status = host.succeed(f"systemctl show -P StatusText microvm@{name}.service")
        assert "leak" not in status, status

    with subtest("no CLIProxyAPI key in the guest"):
        guest("! test -e /run/host-secrets")
        guest("! test -e /run/moot-wright")
        t3_env = guest(f"tr '\\0' '\\n' < /proc/{main_pid('t3-' + name)}/environ")
        assert "TS_EGRESS_PLACEHOLDER_GITHUB_TOKEN" in t3_env, t3_env
        assert f"HTTPS_PROXY=http://{tap_host}:1056" in t3_env, t3_env
        trace = wright("strace -f -qq -v -s 512 -e trace=execve codex --version 2>&1")
        assert "CLIPROXY_API_KEY=tailnet" in trace, trace
        trace = wright("strace -f -qq -v -s 512 -e trace=execve claude --version 2>&1")
        assert "ANTHROPIC_AUTH_TOKEN=tailnet" in trace, trace
        assert "ANTHROPIC_BASE_URL=https://moot-llm.example.ts.net" in trace, trace
        for d in ["pi", "pi-t3"]:
            models = json.loads(guest(f"cat {home}/.config/{d}/agent/models.json"))
            cliproxy = models["providers"]["cliproxy"]
            assert cliproxy["baseUrl"] == "https://moot-llm.example.ts.net", cliproxy["baseUrl"]
            assert cliproxy["apiKey"] == "tailnet", cliproxy["apiKey"]

    with subtest("no secret reaches the store or a managed config"):
        # grep exits 1 only when it read everything and matched nothing.
        guest(f"rc=0; grep -rlFs -e {authkey} /nix/store /etc {home}/.config {home}/.t3 || rc=$?; test $rc -eq 1")

    with subtest("all four providers, T3 and harness-plugins-sync are configured"):
        for tool in ["claude", "codex", "pi", "agy", "t3", "harness-plugins-sync"]:
            wright(f"command -v {tool}")
        for tool in ["claude", "codex", "pi"]:
            wright(f"timeout 120 {tool} --version")
        settings = json.loads(guest(f"cat {home}/.t3/userdata/settings.json"))
        assert settings["linkedEnvironmentsEnabled"] is True
        assert sorted(settings["providerInstances"]) == ["antigravity", "claudeAgent", "codex", "pi"], settings
        agy = settings["providerInstances"]["antigravity"]["config"]["binaryPath"]
        guest(f"test -x {agy}")
        codex = guest(f"cat {home}/.config/codex/config.toml")
        for want in ['model_provider = "cliproxy"', 'base_url = "https://moot-llm.example.ts.net/v1"', 'wire_api = "responses"', 'env_key = "CLIPROXY_API_KEY"']:
            assert want in codex, codex
        claude = json.loads(guest(f"cat {home}/.config/claude/settings.json"))
        assert claude["env"]["ANTHROPIC_BASE_URL"] == "https://moot-llm.example.ts.net", claude
        edn = guest(f"cat {home}/.config/ol-plugins-sync/config.edn")
        for want in [":discover-t3 true", ":peers []", '"harness-plugins" {:url']:
            assert want in edn, edn
        wright("systemctl --user is-active harness-plugins-sync.timer")

    with subtest("managed settings merge over the wright's own"):
        t3_path = f"{home}/.t3/userdata/settings.json"
        settings = json.loads(guest(f"cat {t3_path}"))
        mine_env = {"name": "MINE", "value": "1", "sensitive": False}
        settings["mine"] = 1
        instances = settings["providerInstances"]
        instances["custom"] = {}
        instances["codex"]["enabled"] = False
        instances["pi"].update(
            displayName="My Pi",
            enabled=False,
            environment=[{"name": "PI_CODING_AGENT_DIR", "value": "/stale", "sensitive": False}, mine_env],
        )
        instances["antigravity"]["config"]["binaryPath"] = "/nix/store/stale-agy"
        write_json(t3_path, settings)
        for d in ["pi", "pi-t3"]:
            write_json(f"{home}/.config/{d}/agent/settings.json", {"theme": "dark", "defaultModel": "mine/model", "packages": ["/stale"]})
            models = json.loads(guest(f"cat {home}/.config/{d}/agent/models.json"))
            models["providers"]["local"] = {"baseUrl": "http://127.0.0.1:8000"}
            models["providers"]["cliproxy"]["baseUrl"] = "https://stale"
            write_json(f"{home}/.config/{d}/agent/models.json", models)
        wright("printf '[projects.\"/x\"]\\ntrust_level = \"trusted\"\\n' >> ~/.config/codex/config.toml")
        guest("systemctl restart moot-wright-config.service")

        settings = json.loads(guest(f"cat {t3_path}"))
        instances = settings["providerInstances"]
        assert settings["mine"] == 1 and "custom" in instances, settings
        assert settings["linkedEnvironmentsEnabled"] is True
        assert instances["codex"]["enabled"] is False, instances
        pi_t3 = {"name": "PI_CODING_AGENT_DIR", "value": f"{home}/.config/pi-t3/agent", "sensitive": False}
        assert instances["pi"]["displayName"] == "My Pi" and instances["pi"]["enabled"] is False, instances
        assert instances["pi"]["environment"] == [mine_env, pi_t3], instances
        assert instances["antigravity"]["config"]["binaryPath"] == agy, instances
        for d, profile in [("pi", "pi-cli"), ("pi-t3", "pi")]:
            pi_settings = json.loads(guest(f"cat {home}/.config/{d}/agent/settings.json"))
            assert pi_settings["theme"] == "dark" and pi_settings["defaultModel"] == "mine/model", pi_settings
            assert pi_settings["packages"] == [f"{home}/.local/share/ol-plugins-sync/profiles/{profile}"], pi_settings
            assert pi_settings["defaultProvider"] == "cliproxy", pi_settings
            providers = json.loads(guest(f"cat {home}/.config/{d}/agent/models.json"))["providers"]
            assert "local" in providers, providers
            assert providers["cliproxy"]["baseUrl"] == "https://moot-llm.example.ts.net", providers
        codex = guest(f"cat {home}/.config/codex/config.toml")
        assert "trust_level" in codex and 'model_provider = "cliproxy"' in codex, codex

    with subtest("the wright's identity"):
        assert wright("git config user.name").strip() == name
        assert wright("git config user.email").strip() == f"{name}@moot.land"
        assert wright("git config init.defaultBranch").strip() == "main"
        assert wright("echo $MOOT_MATRIX_ID").strip() == f"@{name}:moot.land"

    with subtest("deploy still hot-switches the guest over VSOCK"):
        host.succeed(f"timeout 600 ${cfg.microvm.deploy.sshSwitch}/bin/microvm-switch {target}")
        assert guest("cat /proc/sys/kernel/random/boot_id").strip() == boot_id, "the guest rebooted"
        guest("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")

    with subtest("the node runs only while the host firewall is loaded"):
        host.succeed("systemctl reload nftables.service")
        assert guest("cat /proc/sys/kernel/random/boot_id").strip() == boot_id, "a firewall reload restarted the guest"
        host.succeed("systemctl stop nftables.service")
        host.fail("nft list table inet moot-nodes")
        for unit in [f"moot-node-netns@{name}", f"microvm@{name}", f"microvm-tap-interfaces@{name}", f"moot-node@{name}"]:
            host.fail(f"systemctl is-active {unit}.service")
        host.fail(f"ip netns list | grep -qw {netns}")
        host.succeed("systemctl start nftables.service")
        host.succeed(f"systemctl start microvm@{name}.service")
        host.wait_until_succeeds(f"{ssh} true", timeout=600)
        host.wait_for_unit(node)
        host.succeed(f"{in_node} nft list table inet moot-node")
        guest_fail(tcp("198.51.100.10", 80))
        guest_fail(tcp(tap_host, 8080))
  '';
}
