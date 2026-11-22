{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.global.domain) tailnet;
  cliproxyUrl = "https://quine.${tailnet}:8317";

  t3Package = pkgs.t3code.override {
    providerPackages = [ config.modules.dev.llms.claude.package ];
  };

  instances = {
    alice = {
      t3Uid = 2001;
      tsUid = 2101;
      hostAddr = "192.168.30.1/30";
      nsAddr = "192.168.30.2/30";
      # Names of the credentials alice's egress grant injects.
      credentials = [
        "GITHUB_TOKEN"
        "DUMMY_TOKEN"
      ];
    };
    bob = {
      t3Uid = 2002;
      tsUid = 2102;
      hostAddr = "192.168.30.5/30";
      nsAddr = "192.168.30.6/30";
      credentials = [ ];
    };
  };

  # Each user has its own loopback, so both use the same ports.
  proxyPort = 1056;
  t3Port = 3780;

  proxyEnv =
    let
      proxy = "http://127.0.0.1:${toString proxyPort}";
      noProxy = "localhost,127.0.0.1,::1";
    in
    {
      HTTPS_PROXY = proxy;
      HTTP_PROXY = proxy;
      https_proxy = proxy;
      http_proxy = proxy;
      NO_PROXY = noProxy;
      no_proxy = noProxy;
    };

  # The egress signs decrypted flows with its own CA. Node keeps its own
  # roots, so it needs the CA named on top of the system bundle.
  egressCa = ../../config/ts-egress-ca.pem;
  caEnv = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    NODE_EXTRA_CA_CERTS = "${egressCa}";
  };
  # Tenants hold only placeholders; the egress swaps in the secret on hosts
  # whose rule names it. The format is policy.PlaceholderPrefix in
  # pkgs/ts-egress.
  placeholderEnv = i: lib.genAttrs i.credentials (n: "TS_EGRESS_PLACEHOLDER_${n}");
  tenantEnv = i: proxyEnv // userEnv i // caEnv // placeholderEnv i;

  # systemctl --user and systemd-run --user need the bus address under
  # runuser or a system service, not only XDG_RUNTIME_DIR.
  userEnv = i: {
    XDG_RUNTIME_DIR = "/run/user/${toString i.t3Uid}";
    DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString i.t3Uid}/bus";
  };
  exports =
    env:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg v}") env);

  netns = name: "t3-${name}";
  hostIface = name: "t3-${name}-host";
  netnsUnits = name: [
    "systemd-netns-private-${netns name}.service"
    "systemd-netns-private-access-${netns name}.service"
  ];
  hostIfaces = "{ ${
    lib.concatMapStringsSep ", " (name: "\"${hostIface name}\"") (lib.attrNames instances)
  } }";

  # The egress MicroVM: its exit node carries tenant traffic, and its CONNECT
  # helper carries ts-plug's own control and DERP connections.
  egressAddr = lib.head config.site.net.svc.hosts4.egress;
  egressHelperPort = 3128;
  egressWireguardPort = 41641;
  # Tenant DNS is captured like any other UDP and answered by the egress.
  dnsResolver = "1.1.1.1";
  transparentAddr = "127.0.0.1:15001";
  captureMark = "0x1";
  captureTable = "100";

  # Every user in the namespace but ts-plug has its non-local TCP and UDP
  # steered into ts-plug's transparent listeners. With ts-plug down the
  # steered flows never leave the namespace: TCP gets a reset, and UDP is
  # dropped, because the kernel cannot send an ICMP error from the original
  # non-local destination.
  captureRules =
    name: i:
    pkgs.writeText "t3-${name}-capture.nft" ''
      table inet t3-capture
      delete table inet t3-capture
      table inet t3-capture {
        chain output {
          type route hook output priority mangle; policy accept;
          meta skuid ${toString i.tsUid} return
          fib daddr type local return
          meta nfproto ipv6 reject
          meta l4proto { tcp, udp } meta mark set ${captureMark}
        }
        chain prerouting {
          type filter hook prerouting priority mangle; policy accept;
          iifname != "lo" return
          meta mark ${captureMark} meta l4proto { tcp, udp } tproxy ip to ${transparentAddr} accept
          meta mark ${captureMark} meta l4proto tcp reject with tcp reset
          meta mark ${captureMark} drop
        }
      }
    '';

  tenantUnits = name: i: [
    "ts-plug-${name}"
    "t3-${name}"
    "user@${toString i.t3Uid}"
  ];
  # The root-ns drops live in nftables and the capture in the namespace's
  # access unit, so tenants stop with either.
  confinement = name: {
    bindsTo = [ "nftables.service" ] ++ netnsUnits name;
    after = [ "nftables.service" ] ++ netnsUnits name;
  };

  perInstance = f: lib.mkMerge (lib.mapAttrsToList f instances);
in
{
  imports = [ ../../modules/networking/systemd-netns-private.nix ];

  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "quine";
    hostFQDN = "quine.${config.site.net.mgmt.domainName}";
    bootstrapSops.enable = true;
    nixStoreBackend = "blk";
    mounts = [
      "var/lib"
      "home"
    ];
  };
  microvm = {
    # bootstrapSops passes the host key as a credential, which
    # cloud-hypervisor does not support.
    hypervisor = "qemu";
    vcpu = 4;
    mem = 6144;
  };

  modules.users.enable = lib.mkForce false;
  modules.services.sshd.enable = lib.mkForce false;
  security.sudo.enable = false;

  sops.defaultSopsFile = ./secrets.sops.yaml;

  security.pki.certificateFiles = [ egressCa ];

  environment.systemPackages = [ pkgs.socat ];
  environment.extraInit = ''
    case "$(id -un)" in
    ${lib.concatStrings (
      lib.mapAttrsToList (name: i: ''
        t3-${name})
        ${exports (tenantEnv i)}
        ;;
      '') instances
    )}
    esac
  '';

  modules.networking.systemd-netns-private = {
    enable = true;
    namespaces = lib.mapAttrs' (
      name: i:
      lib.nameValuePair (netns name) {
        inherit (i) hostAddr nsAddr;
        hostIface = hostIface name;
        nsIface = "t3-${name}-ns";
        natInterface = "svc";
        services = [
          "ts-plug-${name}.service"
          "t3-${name}.service"
        ];
      }
    ) instances;
  };
  # glibc would resolve through nsncd or resolved's NSS socket, both in the
  # root namespace, so tenant lookups would skip the egress DNS filter and
  # the egress would never learn names for raw TCP/UDP rules. Resolve only
  # from files and each namespace's resolv.conf.
  services.nscd.enable = false;
  system.nssModules = lib.mkForce [ ];
  system.nssDatabases.hosts = lib.mkForce [
    "files"
    "dns"
  ];

  environment.etc."resolv-external.conf".text = ''
    nameserver ${dnsResolver}
  '';

  networking.nftables.enable = true;
  networking.nftables.tables.t3-tenants = {
    family = "inet";
    content = ''
      chain input {
        type filter hook input priority filter; policy accept;
        iifname ${hostIfaces} counter drop
      }
      # Only ts-plug's own traffic leaves the namespaces: the CONNECT helper
      # and WireGuard to the egress.
      chain forward {
        type filter hook forward priority filter; policy accept;
        iifname ${hostIfaces} oifname "svc" ip daddr ${egressAddr} tcp dport ${toString egressHelperPort} accept
        iifname ${hostIfaces} oifname "svc" ip daddr ${egressAddr} udp dport ${toString egressWireguardPort} accept
        iifname ${hostIfaces} counter drop
        oifname ${hostIfaces} ct state != { established, related } counter drop
      }
    '';
  };

  users = perInstance (
    name: i: {
      groups."t3-${name}".gid = i.t3Uid;
      groups."ts-${name}".gid = i.tsUid;
      users."t3-${name}" = {
        isNormalUser = true;
        uid = i.t3Uid;
        group = "t3-${name}";
        home = "/home/t3-${name}";
        # T3 starts agents in scopes under this user's systemd manager.
        linger = true;
      };
      users."ts-${name}" = {
        isSystemUser = true;
        uid = i.tsUid;
        group = "ts-${name}";
      };
    }
  );

  systemd.tmpfiles.rules = lib.mapAttrsToList (
    name: _: "d /home/t3-${name} 0700 t3-${name} t3-${name} -"
  ) instances;

  sops.secrets = lib.mkMerge [
    { cliproxy_key = { }; }
    (perInstance (name: _: { "ts_authkey_${name}" = { }; }))
  ];
  sops.templates = perInstance (
    name: _: {
      # tsnet reads TS_AUTHKEY; the key is only used for the first login.
      "ts-${name}.env" = {
        owner = "ts-${name}";
        content = ''
          TS_AUTHKEY=${config.sops.placeholder."ts_authkey_${name}"}
        '';
      };
      "t3-${name}.env" = {
        owner = "t3-${name}";
        content = ''
          ANTHROPIC_AUTH_TOKEN=${config.sops.placeholder.cliproxy_key}
        '';
      };
    }
  );

  systemd.services = lib.mkMerge [
    (perInstance (
      name: i: {
        "systemd-netns-private-access-${netns name}".serviceConfig.ExecStartPost =
          let
            inNs = "${pkgs.iproute2}/bin/ip netns exec ${netns name}";
            ip = "${pkgs.iproute2}/bin/ip";
          in
          [
            "${inNs} ${lib.getExe pkgs.nftables} -f ${captureRules name i}"
            "-${inNs} ${ip} rule del fwmark ${captureMark} lookup ${captureTable}"
            "${inNs} ${ip} rule add fwmark ${captureMark} lookup ${captureTable}"
            "${inNs} ${ip} route replace local 0.0.0.0/0 dev lo table ${captureTable}"
          ];
      }
    ))
    (perInstance (name: i: lib.genAttrs (tenantUnits name i) (_: confinement name)))
    (perInstance (
      name: i: {
        # Agents and user services run under this manager, so it joins the
        # user's namespace too.
        "user@${toString i.t3Uid}" = {
          overrideStrategy = "asDropin";
          serviceConfig = {
            NetworkNamespacePath = "/var/run/netns/${netns name}";
            BindReadOnlyPaths = [ "/etc/netns/${netns name}/resolv.conf:/etc/resolv.conf:norbind" ];
          };
        };
      }
    ))
    (perInstance (
      name: i: {
        "ts-plug-${name}" = {
          description = "ts-plug tailnet node for t3-${name}";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [
            "network-online.target"
            "sops-install-secrets.service"
          ];
          # Control and DERP go through the egress's CONNECT helper, the only
          # TCP the root namespace forwards. HTTP_PROXY stays unset, since
          # control's port 80 attempt would send it a plain request.
          environment = {
            HTTPS_PROXY = "http://${egressAddr}:${toString egressHelperPort}";
            TS_FORCE_NOISE_443 = "true";
            TS_NO_LOGS_NO_SUPPORT = "true";
          };
          serviceConfig = {
            User = "ts-${name}";
            Group = "ts-${name}";
            StateDirectory = "ts-plug-${name}";
            StateDirectoryMode = "0700";
            EnvironmentFile = config.sops.templates."ts-${name}.env".path;
            ExecStart = lib.concatStringsSep " " [
              (lib.getExe pkgs.ts-plug)
              "-hostname t3-${name}"
              "-dir /var/lib/ts-plug-${name}"
              "-https-port 443:${toString t3Port}"
              # T3 streams over websockets and long responses.
              "-response-header-timeout 0"
              "-http-proxy 127.0.0.1:${toString proxyPort}"
              "-exit-node egress"
              "-dns-resolver ${dnsResolver}:53"
              "-transparent-tcp ${transparentAddr}"
              "-transparent-udp ${transparentAddr}"
            ];
            # IP_TRANSPARENT needs CAP_NET_RAW or CAP_NET_ADMIN; NET_RAW cannot
            # change the namespace's capture rules or routes.
            # UDP replies leave from the original destination, so
            # ports such as 53 and 123 need CAP_NET_BIND_SERVICE.
            AmbientCapabilities = [
              "CAP_NET_RAW"
              "CAP_NET_BIND_SERVICE"
            ];
            CapabilityBoundingSet = [
              "CAP_NET_RAW"
              "CAP_NET_BIND_SERVICE"
            ];
            Restart = "on-failure";
            RestartSec = 5;
          };
        };

        "t3-${name}" = {
          description = "T3 Code server for t3-${name}";
          wantedBy = [ "multi-user.target" ];
          # T3 probes for agent scopes once at startup, so the user manager
          # must already be running.
          after = [
            "ts-plug-${name}.service"
            "user@${toString i.t3Uid}.service"
          ];
          wants = [
            "ts-plug-${name}.service"
            "user@${toString i.t3Uid}.service"
          ];
          path = [
            t3Package
            config.modules.dev.llms.claude.package
            pkgs.git
            pkgs.bash
          ];
          environment = tenantEnv i // {
            HOME = "/home/t3-${name}";
            T3CODE_HOME = "/home/t3-${name}/.t3";
            CLAUDE_CONFIG_DIR = "/home/t3-${name}/.config/claude";
            ANTHROPIC_BASE_URL = cliproxyUrl;
            CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
            DISABLE_AUTOUPDATER = "1";
          };
          preStart = ''
            settings=/home/t3-${name}/.t3/userdata/settings.json
            mkdir -p "$(dirname "$settings")"
            [ -s "$settings" ] || echo '{}' > "$settings"
            ${lib.getExe pkgs.jq} '.linkedEnvironmentsEnabled = true' "$settings" > "$settings.new"
            mv "$settings.new" "$settings"
          '';
          serviceConfig = {
            User = "t3-${name}";
            Group = "t3-${name}";
            WorkingDirectory = "/home/t3-${name}";
            EnvironmentFile = config.sops.templates."t3-${name}.env".path;
            ExecStart = "${lib.getExe' t3Package "t3"} serve --host 127.0.0.1 --port ${toString t3Port} /home/t3-${name}";
            Restart = "on-failure";
            RestartSec = 5;
          };
        };
      }
    ))
  ];
}
