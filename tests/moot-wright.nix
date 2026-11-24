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
  untagged = mkGuest { modules.microvm-guest.mootWright.tailscale.advertiseTags = lib.mkForce [ ]; };
  cfg = guest.config;
  wright = cfg.modules.microvm-guest.mootWright;
  inherit (wright) name network;
  service = n: cfg.systemd.services.${n};
  gitConfig = lib.foldl' lib.recursiveUpdate { } cfg.programs.git.config;
  tsPlugScript = c: c.systemd.services."ts-plug-${name}".serviceConfig.ExecStart;
  confined = [
    "ts-plug-${name}"
    "t3-${name}"
    "user@2001"
    "nix-daemon"
  ];
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
  tapFailures =
    hostAddress: guestAddress: prefixLength:
    failures "mootWright"
      (mkGuest {
        modules.microvm-guest.mootWright.network = {
          hostAddress = lib.mkForce hostAddress;
          guestAddress = lib.mkForce guestAddress;
          prefixLength = lib.mkForce prefixLength;
        };
      }).config;
  tapHost =
    guests:
    mkBareHost {
      systemd.network.enable = true;
      modules.microvm-host.isolatedTaps = {
        uplinks = [ "eth1" ];
        inherit guests;
      };
    };
  tapHostFailures = guests: failures "isolatedTaps" (tapHost guests);
  disjointHost = tapHost disjointTaps;
  disjointTaps = {
    a = {
      tapId = "tap-a";
      hostAddress = "10.0.0.1";
    };
    b = {
      tapId = "tap-b";
      hostAddress = "10.0.0.5";
    };
  };
in
assert lib.assertMsg
  (
    cfg.microvm.hypervisor == "cloud-hypervisor"
    && cfg.microvm.storeOnDisk
    && cfg.modules.microvm-guest.zvols.nix-rw.size == wright.zvolSize
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
) "the wright's only interface is its host-local tap";
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
assert lib.assertMsg (lib.all
  (n: builtins.elem "/run/host-secrets" (service n).unitConfig.RequiresMountsFor)
  [
    "ts-plug-${name}"
    "moot-wright-credentials"
    "nix-daemon"
  ]
) "every host-secrets consumer waits for the share";
assert lib.assertMsg (lib.all (
  n:
  builtins.elem "nftables.service" (service n).bindsTo
  && builtins.elem "systemd-netns-private-access-moot-${name}.service" (service n).bindsTo
  && builtins.elem "systemd-netns-private-access-moot-${name}.service" (service n).after
) confined) "confined units start after and stop with nftables and the namespace";
assert lib.assertMsg (
  cfg.nix.settings.max-jobs == 0
  && cfg.nix.settings.extra-substituters == [ "https://moot-cache.example.ts.net" ]
  && map (m: m.protocol) cfg.nix.buildMachines == [ "ssh-ng" ]
  && cfg.nix.distributedBuilds
) "builds go only to the ssh-ng remote builder, with the moot cache as substituter";
assert lib.assertMsg (
  gitConfig.user.email == "${name}@moot.land"
  && gitConfig.user.name == name
  && wright.identity.matrixId == "@${name}:moot.land"
) "the wright's identity defaults to its name";
assert lib.assertMsg (
  lib.any (lib.hasInfix "namespace link") (tapFailures "192.168.255.1" "192.168.255.2" 23)
  && tapFailures "192.168.254.6" "192.168.254.5" 30 == [ ]
  && lib.any (lib.hasInfix "distinct host addresses") (tapFailures "10.77.0.1" "10.77.0.5" 30)
  && lib.any (lib.hasInfix "distinct host addresses") (tapFailures "10.77.0.0" "10.77.0.2" 30)
) "the guest tap subnet must miss the namespace link and hold both tap addresses";
assert lib.assertMsg (
  failures "isolatedTaps" disjointHost == [ ]
  && lib.any (lib.hasInfix "share tap ids: tap-a") (
    tapHostFailures (lib.recursiveUpdate disjointTaps { b.tapId = "tap-a"; })
  )
  && lib.any (lib.hasInfix "overlapping subnets: a/b") (
    tapHostFailures (lib.recursiveUpdate disjointTaps { a.prefixLength = 29; })
  )
) "isolated taps need unique tap ids and disjoint subnets";
assert lib.assertMsg (lib.all
  (
    u:
    builtins.elem "nftables.service" u.bindsTo
    && builtins.elem "nftables.service" u.after
    && u.overrideStrategy == "asDropin"
  )
  (
    map (n: disjointHost.systemd.services.${n}) [
      "microvm@a"
      "microvm-tap-interfaces@a"
    ]
  )
) "isolated guests and their taps stop with the host firewall";
assert lib.assertMsg (
  !bareHost.networking.nftables.enable
  && !(bareHost.networking.nftables.tables ? microvm-isolated-taps)
  && !(bareHost.boot.kernel.sysctl ? "net.ipv4.conf.all.forwarding")
) "a host without isolated taps keeps its firewall and sysctls";
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
      isolatedTaps = {
        uplinks = [ "eth1" ];
        guests.${name} = {
          inherit (network) tapId hostAddress;
        };
      };
    };
    microvm.vms.${name} = {
      evaluatedConfig = guest;
      autostart = false;
    };
    networking.firewall.trustedInterfaces = [ network.tapId ];
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
    environment.systemPackages = [ pkgs.socat ];
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
    ts_uid = 2101
    netns = f"moot-{name}"
    state = f"/var/lib/microvms/{name}"
    target = f"root@vsock-mux/{state}/notify.vsock"
    ssh = f"ssh -n {target}"
    authkey = "tskey-auth-moot-wright-test-0b5e"
    proxy_key = "cliproxy-moot-wright-test-7d21"
    home = f"/home/{name}"
    in_ns = f"ip netns exec {netns}"
    as_wright = f"setpriv --reuid={uid} --regid={uid} --init-groups"
    as_ts = f"setpriv --reuid={ts_uid} --regid={ts_uid} --clear-groups"


    def guest(cmd):
        return host.succeed(f"{ssh} {shlex.quote(cmd)}")


    def guest_fail(cmd):
        host.fail(f"{ssh} {shlex.quote(cmd)}")


    def wright(cmd):
        return guest(f"runuser -l {name} -c {shlex.quote(cmd)}")


    def tcp(prefix, addr, port, opts=""):
        return f"{prefix} socat -T3 - TCP:{addr}:{port},connect-timeout=3{opts} </dev/null"


    def write_json(path, value):
        data = base64.b64encode(json.dumps(value).encode()).decode()
        wright(f"echo {data} | base64 -d > {path}")


    def main_pid(unit):
        return guest(f"systemctl show -P MainPID {unit}").strip()


    def drop_count(chain, match):
        rules = json.loads(host.succeed(f"nft -j list chain inet microvm-isolated-taps {chain}"))["nftables"]
        for r in rules:
            expr = r.get("rule", {}).get("expr", [])
            text = json.dumps(expr)
            if match in text and any("drop" in e for e in expr):
                return next(e["counter"]["packets"] for e in expr if "counter" in e)
        raise Exception(f"no drop rule matching {match} in {chain}")


    lan.start()
    host.start()
    lan.wait_for_unit("peer-echo.service")
    lan.wait_for_unit("udp-sink.service")
    lan.wait_for_unit("dns-sink.service")
    host.wait_for_unit("multi-user.target")
    # As on the real hosts: virtiofs checks a non-root guest user against POSIX ACLs.
    host.succeed("zpool create -O mountpoint=none -O acltype=posixacl -O xattr=sa tank /dev/vdb")
    host.succeed(
        f"install -d -m 0700 /run/microvms/secrets/{name}"
        f" && printf %s {authkey} > /run/microvms/secrets/{name}/ts-authkey"
        f" && printf %s {proxy_key} > /run/microvms/secrets/{name}/cliproxy-key"
        f" && install -m 0400 /etc/microvm-test-key /run/microvms/secrets/{name}/nix-builder-key"
        f" && chmod 0400 /run/microvms/secrets/{name}/*"
    )
    host.succeed(f"systemctl start microvm@{name}.service")
    host.wait_until_succeeds(f"{ssh} true", timeout=600)
    guest("systemctl is-system-running --wait || true")
    boot_id = guest("cat /proc/sys/kernel/random/boot_id").strip()

    with subtest("the guest is reachable only over VSOCK"):
        host.succeed("ip -4 addr show ${network.tapId} | grep -q '${network.hostAddress}/30'")
        guest("ip -4 addr show uplink | grep -q '${network.guestAddress}/30'")
        guest("! ss -Htln | grep -q ':22 '")
        guest("test -s /var/lib/ssh/ssh_host_ed25519_key")
        guest("findmnt -n -o FSTYPE /nix/.rw-store | grep -qx ext4")

    with subtest("the wright, T3, ts-plug and nix-daemon run in the wright's namespace"):
        guest("systemctl start moot-wright-config.service t3-${name}.service")
        guest(f"systemctl is-active t3-{name}.service ts-plug-{name}.service user@{uid}.service")
        guest("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        root_ns = guest("readlink /proc/1/ns/net").strip()
        for unit in [f"t3-{name}", f"ts-plug-{name}", f"user@{uid}", "nix-daemon"]:
            pid = main_pid(unit)
            assert guest(f"ip netns identify {pid}").strip() == netns, unit
            assert guest(f"readlink /proc/{pid}/ns/net").strip() != root_ns, unit
        listening = shlex.quote(f"{in_ns} ss -Htln | grep -q 127.0.0.1:3780")
        host.wait_until_succeeds(f"{ssh} {listening}", timeout=300)
        guest_fail("ss -Htln | grep -q 127.0.0.1:3780")

    with subtest("nix-daemon serves root and the wright and fetches through ts-plug"):
        guest("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        wright("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        environ = guest(f"tr '\\0' '\\n' < /proc/{main_pid('nix-daemon')}/environ")
        assert "HTTPS_PROXY=http://127.0.0.1:1056" in environ, environ
        conf = guest("cat /etc/nix/nix.conf")
        assert "max-jobs = 0" in conf, conf
        assert "extra-substituters = https://moot-cache.example.ts.net" in conf, conf
        # Nix reads builders from /etc/nix/machines by default.
        assert "builders =" not in conf, conf
        guest("grep -q '^ssh-ng://nix-ssh@moot-builder.example.ts.net ' /etc/nix/machines")

    with subtest("the guest reaches the public internet through the host, masqueraded"):
        out = guest(tcp("", "198.51.100.10", 80))
        assert "peer=192.168.1.1" in out, out
        out = guest(tcp(f"{in_ns} {as_ts}", "198.51.100.10", 80))
        assert "peer=192.168.1.1" in out, out

    with subtest("the host drops guest traffic to itself and to private ranges"):
        host.succeed("socat -T3 - TCP:127.0.0.1:8080 </dev/null | grep -qx host")
        before = drop_count("input", "${network.tapId}")
        guest_fail(tcp("", "${network.hostAddress}", 8080))
        guest_fail(tcp("", "192.168.1.1", 8080))
        assert drop_count("input", "${network.tapId}") > before
        before = drop_count("forward", "daddr")
        guest_fail(tcp("", "192.168.1.2", 80))
        guest_fail(tcp("", "100.100.100.100", 80))
        assert drop_count("forward", "daddr") > before
        lan.succeed("socat -T3 - TCP:192.168.1.2:80 </dev/null | grep -q peer=")

    with subtest("the host drops connections into the guest"):
        guest("systemd-run --unit=guest-echo socat TCP-LISTEN:7000,fork,reuseaddr SYSTEM:'echo guest'")
        guest("sleep 1; socat -T3 - TCP:127.0.0.1:7000 </dev/null | grep -qx guest")
        before = drop_count("forward", "oifname")
        lan.fail("socat -T3 - TCP:10.77.0.2:7000,connect-timeout=3 </dev/null")
        assert drop_count("forward", "oifname") > before

    with subtest("the namespace reaches neither the guest nor private ranges"):
        for addr, port in [("192.168.254.1", 7000), ("${network.guestAddress}", 7000), ("192.168.1.2", 80), ("${network.hostAddress}", 8080)]:
            guest_fail(tcp(f"{in_ns} {as_ts}", addr, port))

    with subtest("with ts-plug stopped, nothing but ts-plug leaves the namespace"):
        guest(f"systemctl stop ts-plug-{name}.service")
        guest(f"systemctl is-active t3-{name}.service user@{uid}.service systemd-netns-private-access-{netns}.service")
        daemon_ns = f"nsenter --net=/proc/{main_pid('nix-daemon')}/ns/net"
        assert guest(f"ip netns identify {main_pid('nix-daemon')}").strip() == netns
        for prefix in [f"{in_ns} {as_wright}", daemon_ns, f"{in_ns}"]:
            out = guest(f"{tcp(prefix, '198.51.100.10', 80)} 2>&1 || true")
            assert "Connection refused" in out, f"{prefix}: {out}"
            guest_fail(f"{prefix} socat -T3 - 'TCP6:[2001:db8::10]:80,connect-timeout=3' </dev/null")
            guest_fail(f"{prefix} ping -c1 -W2 198.51.100.10")
        guest("ping -c1 -W2 198.51.100.10")
        guest(f"echo leak-wright | {in_ns} {as_wright} socat -u - UDP:198.51.100.10:9999")
        guest(f"echo leak-daemon | {daemon_ns} socat -u - UDP:198.51.100.10:9999")
        guest(f"echo ok-ts | {in_ns} {as_ts} socat -u - UDP:198.51.100.10:9999")
        guest("echo ok-root | socat -u - UDP:198.51.100.10:9999")
        bound = [",so-bindtodevice=mw-ns", ",bind=192.168.254.2"]
        out = guest(tcp(f"{in_ns} {as_ts}", "198.51.100.10", 80, bound[0]))
        assert "peer=192.168.1.1" in out, out
        guest(f"echo ok-ts-bound | {in_ns} {as_ts} socat -u - UDP:198.51.100.10:9999{bound[0]}")
        for opts in bound:
            guest_fail(tcp(f"{in_ns} {as_wright}", "198.51.100.10", 80, opts))
            guest(f"echo leak-bound | {in_ns} {as_wright} socat -u - UDP:198.51.100.10:9999{opts} || true")
        lan.wait_until_succeeds("grep -q ok-ts /run/udp-sink && grep -q ok-root /run/udp-sink && grep -q ok-ts-bound /run/udp-sink")
        lan.succeed("sleep 3; ! grep -q leak /run/udp-sink")
        guest(f"systemd-run --unit=capture-probe {in_ns} $(command -v socat) TCP-LISTEN:15001,bind=127.0.0.1,ip-transparent,reuseaddr,fork SYSTEM:'echo captured'")
        guest(f"sleep 1; {tcp(f'{in_ns} {as_wright}', '198.51.100.10', 80)} | grep -qx captured")
        guest_fail(tcp(f"{in_ns} {as_wright}", "198.51.100.10", 80, bound[0]))
        guest("systemctl stop capture-probe")

    with subtest("with ts-plug stopped, the wright resolves nothing through the root namespace"):
        resolve1 = "busctl --timeout=5 call org.freedesktop.resolve1 /org/freedesktop/resolve1 org.freedesktop.resolve1.Manager ResolveHostname isit 0"
        varlink = "varlinkctl --timeout=5 call /run/systemd/resolve/io.systemd.Resolve io.systemd.Resolve.ResolveHostname"
        for prefix in [f"{in_ns} {as_wright}", as_wright]:
            guest(f"timeout 15 {prefix} getent hosts leak-libc.example || true")
            guest(f"timeout 15 {prefix} {resolve1} leak-dbus.example 2 0 || true")
            guest(f"timeout 15 {prefix} {varlink} " + shlex.quote('{"name":"leak-varlink.example","family":2}') + " || true")
        guest("timeout 15 getent hosts ok-root.example || true")
        lan.wait_until_succeeds("grep -aq ok-root /run/dns-sink")
        lan.succeed("sleep 3; ! grep -aq leak /run/dns-sink")

    with subtest("a wright process outside the namespace has no network"):
        guest_fail(tcp(as_wright, "198.51.100.10", 80))
        guest(tcp("", "198.51.100.10", 80))

    with subtest("the wright cannot notify the host's microvm unit over VSOCK"):
        for prefix in [f"{in_ns} {as_wright}", as_wright]:
            guest(f"printf STATUS=leak | timeout 5 {prefix} socat -u - VSOCK-CONNECT:2:8888 || true")
        host.succeed("sleep 2")
        host.fail(f"test -e {state}/notify.vsock_8888")
        host.fail("ss -Hxl | grep -F notify.vsock_8888")
        status = host.succeed(f"systemctl show -P StatusText microvm@{name}.service")
        assert "leak" not in status, status

    with subtest("the wright reads only the proxy key"):
        guest(f"systemctl start ts-plug-{name}.service")
        wright("! ls /run/host-secrets")
        wright("! cat /run/host-secrets/ts-authkey")
        wright("! cat /run/host-secrets/cliproxy-key")
        assert wright("cat /run/moot-wright/cliproxy-key") == proxy_key
        assert guest("stat -c '%U %a' /run/moot-wright/cliproxy-key").strip() == f"{name} 400"
        environ = guest(f"tr '\\0' '\\n' < /proc/{main_pid('ts-plug-' + name)}/environ")
        assert f"TS_AUTHKEY={authkey}" in environ.splitlines(), "ts-plug did not get its auth key"
        t3_env = guest(f"tr '\\0' '\\n' < /proc/{main_pid('t3-' + name)}/environ")
        assert authkey not in t3_env and proxy_key not in t3_env, t3_env
        assert "TS_EGRESS_PLACEHOLDER_GITHUB_TOKEN" in t3_env, t3_env
        cmdline = guest(f"tr '\\0' ' ' < /proc/{main_pid('ts-plug-' + name)}/cmdline")
        assert "-advertise-tags tag:moot-wright" in cmdline, cmdline

    with subtest("the provider wrappers read the key at launch"):
        trace = wright("strace -f -qq -v -s 512 -e trace=execve codex --version 2>&1")
        assert f"CLIPROXY_API_KEY={proxy_key}" in trace, trace
        trace = wright("strace -f -qq -v -s 512 -e trace=execve claude --version 2>&1")
        assert f"ANTHROPIC_AUTH_TOKEN={proxy_key}" in trace, trace
        assert "ANTHROPIC_BASE_URL=https://moot-llm.example.ts.net" in trace, trace
        for d in ["pi", "pi-t3"]:
            models = json.loads(guest(f"cat {home}/.config/{d}/agent/models.json"))
            cliproxy = models["providers"]["cliproxy"]
            assert cliproxy["baseUrl"] == "https://moot-llm.example.ts.net", cliproxy["baseUrl"]
            assert wright(cliproxy["apiKey"].removeprefix("!")) == proxy_key

    with subtest("no secret reaches the store or a managed config"):
        # grep exits 1 only when it read everything and matched nothing.
        guest(f"rc=0; grep -rlFs -e {authkey} -e {proxy_key} /nix/store /etc {home}/.config {home}/.t3 || rc=$?; test $rc -eq 1")

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

    with subtest("a pre-tagged auth key starts ts-plug without -advertise-tags"):
        script = open("${tsPlugScript untagged.config}").read()
        assert "-advertise-tags" not in script, script
        assert "-advertise-tags" in open("${tsPlugScript cfg}").read()

    with subtest("deploy still hot-switches the guest over VSOCK"):
        host.succeed(f"timeout 600 ${cfg.microvm.deploy.sshSwitch}/bin/microvm-switch {target}")
        assert guest("cat /proc/sys/kernel/random/boot_id").strip() == boot_id, "the guest rebooted"
        guest("NIX_REMOTE=daemon nix-store -q --hash /run/current-system")
        assert guest(f"ip netns identify {main_pid('nix-daemon')}").strip() == netns

    with subtest("the guest runs only while the host firewall is loaded"):
        host.succeed("systemctl reload nftables.service")
        assert guest("cat /proc/sys/kernel/random/boot_id").strip() == boot_id, "a firewall reload restarted the guest"
        host.succeed("systemctl stop nftables.service")
        host.fail("nft list table inet microvm-isolated-taps")
        host.fail(f"systemctl is-active microvm@{name}.service")
        host.fail(f"systemctl is-active microvm-tap-interfaces@{name}.service")
        host.fail("ip link show ${network.tapId}")
        host.succeed("systemctl start nftables.service")
        host.succeed(f"systemctl start microvm@{name}.service")
        host.wait_until_succeeds(f"{ssh} true", timeout=600)
        out = guest(tcp("", "198.51.100.10", 80))
        assert "peer=192.168.1.1" in out, out
        guest_fail(tcp("", "${network.hostAddress}", 8080))
  '';
}
