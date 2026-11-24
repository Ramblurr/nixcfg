{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  mkGuest =
    name: hostPath:
    inputs.self.lib.nixcfg.mkGuest name {
      inherit hostPath;
    };
  guest = mkGuest "anvil" ./fixtures/moot-builder;
  wrightGuest = mkGuest "workstead" ./fixtures/moot-wright;
  cfg = guest.config;
  builder = cfg.modules.microvm-guest.mootBuilder;
  inherit (cfg.modules.microvm-guest.mootVm) network;
  name = "anvil";
  netns = "moot-${name}";
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
      address = "192.168.2.1";
      port = 41650;
      allowFrom = [ "192.168.2.3" ];
    };
    builder = {
      allowTags = [ "tag:moot-workstead" ];
      hostKeyFile = "/run/keys/${name}-ssh-host-key";
    };
    authKeyFile = "/run/keys/${name}-ts-authkey";
    inherit (network) dnsResolver;
  };
  daemon = "${network.guestAddress}:${toString builder.daemonPort}";
  mkHost =
    nodes: vms:
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
            mootNodeUplinks = [ "eth1" ];
            mootNodes = nodes;
          };
          microvm.vms = lib.mapAttrs (_: evaluatedConfig: {
            inherit evaluatedConfig;
            autostart = false;
          }) vms;
          networking.hostName = "mootforge";
          system.stateVersion = "26.05";
        }
      ];
    }).config;
  host = mkHost { ${name} = record; } { ${name} = guest; };
  failures =
    c:
    lib.filter (lib.hasInfix "mootNodes") (
      map (a: a.message) (lib.filter (a: !a.assertion) c.assertions)
    );
  envFile = host.environment.etc."moot-nodes/${name}.env".text;

  tlsCert = pkgs.runCommand "headscale-cert" { nativeBuildInputs = [ pkgs.openssl ]; } ''
    mkdir $out
    openssl req -x509 -newkey rsa:2048 -sha256 -days 365 -nodes \
      -out $out/cert.pem -keyout $out/key.pem \
      -subj '/CN=headscale' -addext 'subjectAltName=DNS:headscale'
  '';
  control = "198.51.100.10";
  client = "10.99.0.3";
  peerPort = 41649;
  onControlNet = gateway: {
    security.pki.certificateFiles = [ "${tlsCert}/cert.pem" ];
    networking.hosts.${control} = [ "headscale" ];
    networking.interfaces.eth1.ipv4.routes = [
      {
        address = "198.51.100.0";
        prefixLength = 24;
        via = gateway;
      }
    ];
  };
  policy = pkgs.writeText "policy.hujson" (
    builtins.toJSON {
      tagOwners = lib.genAttrs [
        "tag:moot-egress"
        "tag:moot-workstead"
        "tag:moot-outstead"
        "tag:moot-builder-anvil"
      ] (_: [ "test@" ]);
      autoApprovers.exitNode = [ "tag:moot-egress" ];
      # Both agents reach anvil's SSH, so only anvil's node keeps outstead out.
      acls = [
        {
          action = "accept";
          src = [
            "tag:moot-workstead"
            "tag:moot-outstead"
          ];
          dst = [ "tag:moot-builder-anvil:22" ];
        }
        {
          action = "accept";
          src = [ "tag:moot-builder-anvil" ];
          dst = [ "autogroup:internet:*" ];
        }
      ];
    }
  );
  # Userspace tailscaled nodes on one VM, each with an HTTP proxy like a
  # Moot node's.
  tailnetNodes = {
    ward = 1054;
    workstead = 1055;
    outstead = 1056;
  };
in
assert lib.assertMsg
  (
    cfg.microvm.hypervisor == "cloud-hypervisor"
    && cfg.microvm.storeOnDisk
    && cfg.microvm.balloon
    && cfg.modules.microvm-guest.zvols.nix-rw.size == 4096
    && cfg.microvm.vsock.ssh.enable
    && map (i: i.type) cfg.microvm.interfaces == [ "tap" ]
    && !cfg.modules.microvm-guest.hostSecrets.enable
    && cfg.microvm.credentialFiles == { }
  )
  "the builder runs under cloud-hypervisor with balloon, a zvol store, VSOCK SSH, only its tap and no secrets";
assert lib.assertMsg (
  cfg.systemd.sockets.nix-remote.listenStreams == [ daemon ]
  && cfg.systemd.services."nix-remote@".environment.NIX_REMOTE == "daemon"
  && cfg.nix.settings.max-jobs == 1
  && !(cfg.systemd.services ? "t3-${name}")
  && !cfg.services.tailscale.enable
) "the builder serves only its nix-daemon on the tap, and runs no tailnet node or T3";
assert lib.assertMsg (
  lib.hasInfix "MOOT_SSH_STDIO=${daemon}\n" envFile
  && lib.hasInfix "MOOT_SSH_ALLOW_TAGS=tag:moot-workstead\n" envFile
  && lib.hasInfix "MOOT_T3_PORT=\n" envFile
  &&
    builtins.elem "ssh-host-key:${record.builder.hostKeyFile}"
      host.systemd.services."moot-node@${name}".serviceConfig.LoadCredential
) "a builder's node serves SSH to its tags with its own host key, and no T3";
assert lib.assertMsg (
  failures host == [ ]
  && lib.any (lib.hasInfix "disagrees with its guest on daemonPort") (
    failures (
      mkHost { ${name} = lib.recursiveUpdate record { builder.daemonPort = 7701; }; } { ${name} = guest; }
    )
  )
  && lib.any (lib.hasInfix "is not a Moot builder") (
    failures (mkHost { workstead = record; } { workstead = wrightGuest; })
  )
  && lib.any (lib.hasInfix "is not a Moot wright") (
    failures (mkHost { ${name} = builtins.removeAttrs record [ "builder" ]; } { ${name} = guest; })
  )
) "the host checks a builder record against a builder guest";
pkgs.testers.runNixOSTest {
  name = "moot-builder";
  node.specialArgs = { inherit inputs; };

  nodes.lan = {
    virtualisation.vlans = [
      1
      2
    ];
    networking.firewall.enable = false;
    networking.interfaces.eth1.ipv4.addresses = [
      {
        address = control;
        prefixLength = 24;
      }
      {
        address = "198.18.0.1";
        prefixLength = 15;
      }
    ];
    networking.interfaces.eth1.ipv4.routes = [
      {
        address = client;
        prefixLength = 32;
        via = "192.168.1.1";
      }
    ];
    services.headscale = {
      enable = true;
      port = 8080;
      settings = {
        server_url = "https://headscale";
        derp = {
          server = {
            enabled = true;
            region_id = 999;
            stun_listen_addr = "0.0.0.0:3478";
            ipv4 = control;
          };
          urls = [ ];
        };
        dns = {
          base_domain = "tailnet.test";
          override_local_dns = false;
        };
        policy.path = policy;
      };
    };
    services.nginx = {
      enable = true;
      virtualHosts.headscale = {
        onlySSL = true;
        sslCertificate = "${tlsCert}/cert.pem";
        sslCertificateKey = "${tlsCert}/key.pem";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8080";
          proxyWebsockets = true;
        };
      };
    };
    systemd.services.peer-echo = {
      wantedBy = [ "multi-user.target" ];
      script = ''exec ${lib.getExe pkgs.socat} TCP-LISTEN:80,fork,reuseaddr SYSTEM:'echo "peer=$SOCAT_PEERADDR"' '';
    };
    environment.systemPackages = [ pkgs.headscale ];
  };

  nodes.tailnet = lib.mkMerge [
    (onControlNet "192.168.2.2")
    {
      virtualisation.vlans = [ 2 ];
      virtualisation.memorySize = 1536;
      networking.firewall.enable = false;
      networking.interfaces.eth1.ipv4 = {
        addresses = [
          {
            address = client;
            prefixLength = 32;
          }
        ];
        routes = [
          {
            address = "192.168.1.2";
            prefixLength = 32;
            via = "192.168.2.1";
          }
        ];
      };
      environment.systemPackages = [
        pkgs.tailscale
        pkgs.socat
      ];
      systemd.services =
        lib.mapAttrs' (
          node: port:
          lib.nameValuePair "tailscaled-${node}" {
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              StateDirectory = "tailscale-${node}";
              ExecStart = "${lib.getExe' pkgs.tailscale "tailscaled"} --tun=userspace-networking --port=0 --statedir=/var/lib/tailscale-${node} --socket=/run/tailscale-${node}.sock --outbound-http-proxy-listen=127.0.0.1:${toString port}";
            };
          }
        ) tailnetNodes
        // {
          peer-wg = {
            wantedBy = [ "multi-user.target" ];
            script = "exec ${lib.getExe pkgs.socat} UDP-LISTEN:${toString peerPort},fork,reuseaddr SYSTEM:'echo peer'";
          };
        };
      programs.ssh = {
        knownHosts.${name}.publicKey = sshKeys.snakeOilEd25519PublicKey;
        extraConfig = lib.concatStrings (
          lib.mapAttrsToList (node: port: ''
            Host ${name}-as-${node}
              HostName ${name}.tailnet.test
              HostKeyAlias ${name}
              BatchMode yes
              ProxyCommand ${lib.getExe' pkgs.netcat-openbsd "nc"} -X connect -x 127.0.0.1:${toString port} %h %p
          '') tailnetNodes
        );
      };
    }
  ];

  nodes.host = lib.mkMerge [
    (onControlNet "192.168.1.2")
    {
      imports = [
        inputs.microvm.nixosModules.host
        inputs.impermanence.nixosModules.impermanence
        ../modules/microvm-host
      ];
      virtualisation.vlans = [
        1
        2
      ];
      modules.microvm-host = {
        enable = true;
        baseZfsDataset = "tank/microvms";
        mootNodeUplinks = [
          "eth1"
          "eth2"
        ];
        mootNodePeers = [
          {
            address = "192.168.2.3";
            port = peerPort;
          }
        ];
        mootNodes.${name} = record;
      };
      networking.interfaces.eth1.ipv4.routes = [
        {
          address = "198.18.0.0";
          prefixLength = 15;
          via = "192.168.1.2";
        }
      ];
      networking.interfaces.eth2.ipv4.routes = [
        {
          address = client;
          prefixLength = 32;
          via = "192.168.2.3";
        }
      ];
      microvm.vms.${name} = {
        evaluatedConfig = guest;
        autostart = false;
      };
      systemd.services."moot-node@".environment.TS_CONTROL_URL = "https://headscale";
      boot.supportedFilesystems = [ "zfs" ];
      networking.hostId = "8425e349";
      networking.useNetworkd = true;
      systemd.services.host-echo = {
        wantedBy = [ "multi-user.target" ];
        script = "exec ${lib.getExe pkgs.socat} TCP-LISTEN:8080,fork,reuseaddr SYSTEM:'echo host'";
      };
      environment.systemPackages = [
        pkgs.socat
        pkgs.tailscale
        pkgs.jq
      ];
      virtualisation = {
        cores = 4;
        memorySize = 4096;
        emptyDiskImages = [ 8192 ];
        qemu.options = [
          "-cpu"
          "host"
        ];
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
    }
  ];

  testScript = ''
    import json
    import shlex

    name = "${name}"
    netns = "${netns}"
    state = f"/var/lib/microvms/{name}"
    ssh = f"ssh -n root@vsock-mux/{state}/notify.vsock"
    node = f"moot-node@{name}.service"
    localapi = f"/var/lib/private/moot-nodes/{name}/localapi.sock"
    in_node = f"ip netns exec {netns}"
    tap_host = "${network.hostAddress}"
    guest_addr = "${network.guestAddress}"
    daemon = "${daemon}"
    nix = "nix --extra-experimental-features nix-command"
    drv = 'derivation { name = "moot-remote"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo built > $out" ]; }'


    def guest(cmd):
        return host.succeed(f"{ssh} {shlex.quote(cmd)}")


    def guest_fail(cmd):
        host.fail(f"{ssh} {shlex.quote(cmd)}")


    def tcp(addr, port):
        return f"socat -T5 - TCP:{addr}:{port},connect-timeout=5 </dev/null"


    def ts(node, args):
        return f"tailscale --socket=/run/tailscale-{node}.sock {args}"


    def remote_build(node):
        return (
            f"timeout 120 nix-build --no-out-link --max-jobs 0 --option substituters ''' "
            f"--builders 'ssh-ng://{name}-as-{node} x86_64-linux' -E {shlex.quote(drv)}"
        )


    start_all()
    lan.wait_for_unit("headscale.service")
    lan.wait_for_open_port(443)
    lan.succeed("headscale users create test")


    def authkey(tag):
        out = lan.succeed(f"headscale preauthkeys create -u 1 --reusable --tags {tag} -o json")
        return json.loads(out)["key"]


    with subtest("the agents and ward join the test tailnet"):
        for agent, tag, extra in [
            ("ward", "tag:moot-egress", "--advertise-exit-node"),
            ("workstead", "tag:moot-workstead", ""),
            ("outstead", "tag:moot-outstead", ""),
        ]:
            tailnet.wait_for_unit(f"tailscaled-{agent}.service")
            tailnet.succeed(ts(agent, f"up --login-server=https://headscale --authkey={authkey(tag)} --hostname={agent} {extra}"))

    host.wait_for_unit("multi-user.target")
    host.succeed("zpool create -O mountpoint=none -O acltype=posixacl -O xattr=sa tank /dev/vdb")
    host.succeed("install -d -m 0700 /run/keys")
    host.succeed(f"printf %s {authkey('tag:moot-builder-anvil')} > /run/keys/{name}-ts-authkey")
    host.succeed(f"install -m 0400 ${sshKeys.snakeOilEd25519PrivateKey} /run/keys/{name}-ssh-host-key")
    host.succeed(f"systemctl start microvm@{name}.service")
    host.wait_until_succeeds(f"{ssh} true", timeout=600)
    host.wait_for_unit(node)
    host.wait_until_succeeds(f"tailscale --socket {localapi} status --json | jq -e '.BackendState == \"Running\"'", timeout=300)
    guest("systemctl is-system-running --wait || true")

    with subtest("the node serves SSH to the builder's tags and no T3"):
        cmdline = host.succeed(f"tr '\\0' ' ' < /proc/$(systemctl show -P MainPID {node})/cmdline")
        assert f"-ssh-stdio {daemon} -ssh-allow-tags tag:moot-workstead" in cmdline, cmdline
        assert "-https-port" not in cmdline, cmdline

    with subtest("workstead builds on anvil over ssh-ng with no SSH key"):
        tailnet.succeed("! ls /root/.ssh/id_* 2>/dev/null")
        tailnet.wait_until_succeeds(f"timeout 60 {nix} store info --store ssh-ng://{name}-as-workstead", timeout=300)
        out = tailnet.succeed(remote_build("workstead")).strip()
        assert tailnet.succeed(f"cat {out}").strip() == "built"
        guest(f"test -e {out}")

    with subtest("outstead is refused by anvil's node, though the policy lets it reach port 22"):
        tailnet.fail(f"timeout 60 {nix} store info --store ssh-ng://{name}-as-outstead")
        tailnet.fail(remote_build("outstead").replace("echo built", "echo outstead"))
        host.succeed(f"journalctl -u {node} | grep 'SSH: refused' | grep -q tag:moot-outstead")

    with subtest("the host forwards its node's traffic and nothing else"):
        assert tailnet.succeed("socat -T5 - TCP:192.168.1.2:80,bind=${client},connect-timeout=5 </dev/null || true").strip() == "", "the host routed a LAN client to another segment"
        assert host.succeed(f"{in_node} {tcp('${control}', 80)}").strip() == "peer=192.168.1.1"
        assert host.succeed(f"echo node | {in_node} socat -T5 - UDP:192.168.2.3:${toString peerPort}").strip() == "peer"
        out = host.succeed(f"{in_node} {tcp('198.18.0.1', 80)} || true")
        assert "peer=" not in out, f"the node reached 198.18.0.1: {out}"
        out = tailnet.wait_until_succeeds(ts("workstead", f"ping -c 20 {name}"), timeout=120)
        assert "via 192.168.2.1:${toString record.endpoint.port}" in out, out

    with subtest("the builder holds no keys or tailnet node, and listens only for its node"):
        guest("! pgrep -x tailscaled && ! pgrep -x ts-plug")
        guest("! ls /root/.ssh/id_* 2>/dev/null")
        guest("! test -e /run/host-secrets")
        listeners = guest("ss -Htln").split("\n")
        assert [l.split()[3] for l in listeners if l.strip()] == [daemon], listeners
        host.fail(tcp(guest_addr, 7700))
        lan.fail(tcp(guest_addr, 7700))

    with subtest("root in the builder reaches nothing on the LAN or the host, and the internet only through ward"):
        # The node accepts every guest connection and then dials it through
        # ward, so a refused flow opens and closes with no answer.
        for addr, port, answer in [("192.168.1.2", 80, "peer="), ("192.168.1.1", 8080, "host"), (tap_host, 8080, "host")]:
            out = guest(f"{tcp(addr, port)} || true")
            assert answer not in out, f"{addr}:{port} answered: {out}"
        for addr in ["192.168.1.1", "192.168.1.2", tap_host]:
            guest_fail(f"ping -c1 -W2 {addr}")
        peer = guest(tcp("${control}", 80)).strip()
        assert peer.startswith("peer=192.168.2."), f"internet traffic did not leave through ward: {peer}"
  '';
}
