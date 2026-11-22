{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  evaluate =
    extra:
    lib.nixosSystem {
      modules = [
        ../modules/services/ts-egress.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          systemd.network.enable = true;
          services.resolved.enable = true;
          modules.services.ts-egress = {
            enable = true;
            dnsUpstream = "192.0.2.1:53";
          };
        }
        extra
      ];
    };
  failedAssertions =
    system: map (a: a.message) (lib.filter (a: !a.assertion) system.config.assertions);

  full = evaluate {
    modules.services.ts-egress = {
      selfAddresses = [ "192.0.2.26" ];
      helper = {
        address = "192.0.2.26";
        interface = "svc";
        allowFrom = [ "192.0.2.0/24" ];
      };
      ca.enable = true;
      onepassword.url = "http://op-connect.example.ts.net:8080";
      broker.socket = "/run/ts-broker/decide.sock";
    };
  };
  minimal = evaluate { };
  trusted = evaluate { networking.firewall.trustedInterfaces = [ "tailscale0" ]; };
  halfOnepassword = evaluate {
    modules.services.ts-egress.onepassword.url = "http://op-connect.example.ts.net:8080";
  };
  halfBroker = evaluate {
    modules.services.ts-egress = {
      ca.enable = true;
      broker.socket = "/run/ts-broker/decide.sock";
    };
  };
  # A credential source in the world-readable store is a type error.
  storeKey = evaluate { modules.services.ts-egress.ca.keyFile = "${../config/ts-egress-ca.pem}"; };

  inherit (full) config;
  ruleset = config.networking.nftables.tables.ts-egress.content;
  inputRules = config.networking.firewall.extraInputRules;
  unit = config.systemd.services.ts-egress;
  exec = unit.serviceConfig.ExecStart;
  configFile = lib.last (lib.splitString " " exec);
  minimalExec = minimal.config.systemd.services.ts-egress.serviceConfig.ExecStart;
  minimalConfigFile = lib.last (lib.splitString " " minimalExec);
  tproxyNet = config.systemd.network.networks."40-ts-egress-tproxy";
in
assert failedAssertions full == [ ];
assert failedAssertions minimal == [ ];
assert failedAssertions trusted != [ ];
assert failedAssertions halfOnepassword != [ ];
assert failedAssertions halfBroker != [ ];
assert unit.serviceConfig.SupplementaryGroups == [ "ts-broker-clients" ];
assert config.users.groups ? ts-broker-clients;
assert minimal.config.systemd.services.ts-egress.serviceConfig.SupplementaryGroups == [ ];
assert !(builtins.tryEval storeKey.config.modules.services.ts-egress.ca.keyFile).success;

# Every forwarded TCP and UDP flow from the tailnet reaches ts-egress, and
# nothing is routed past it.
assert lib.hasInfix
  ''iifname "tailscale0" meta l4proto { tcp, udp } tproxy to :15001 meta mark set 1 accept''
  ruleset;
assert lib.hasInfix ''iifname "tailscale0" counter drop'' ruleset;
assert lib.hasInfix ''oifname "tailscale0" counter drop'' ruleset;
assert lib.hasInfix
  ''iifname "tailscale0" meta mark != 1 ct state != { established, related } counter drop''
  ruleset;
assert config.boot.kernel.sysctl."net.ipv4.conf.all.forwarding" == false;
assert config.boot.kernel.sysctl."net.ipv6.conf.all.forwarding" == false;
assert
  tproxyNet.routingPolicyRules == [
    {
      FirewallMark = 1;
      Table = 100;
      Priority = 100;
      Family = "both";
    }
  ];
assert
  map (r: r.Destination) tproxyNet.routes == [
    "0.0.0.0/0"
    "::/0"
  ];

# Only TPROXY'd tailnet traffic and the helper port from the LAN are let in.
assert lib.hasInfix ''iifname "tailscale0" meta mark 1 accept'' inputRules;
assert lib.hasInfix ''iifname "svc" ip saddr { 192.0.2.0/24 } tcp dport 3128 accept'' inputRules;
assert !(lib.hasInfix "3128" minimal.config.networking.firewall.extraInputRules);

# tailscaled cannot answer peerapi DoH from the system resolver.
assert
  config.systemd.services.tailscaled.serviceConfig.TemporaryFileSystem == [
    "/run/systemd/resolve:ro"
  ];
assert lib.all (flag: builtins.elem flag config.services.tailscale.extraUpFlags) [
  "--advertise-exit-node"
  "--advertise-tags=tag:egress"
  "--accept-dns=false"
];

# The service runs after tailscaled with its listeners, helper and credential.
assert builtins.elem "tailscaled.service" unit.requires;
assert builtins.elem "tailscaled.service" unit.after;
assert unit.serviceConfig.Restart == "always";
assert
  unit.serviceConfig.AmbientCapabilities == [
    "CAP_NET_RAW"
    "CAP_NET_BIND_SERVICE"
  ];
assert lib.any (lib.hasInfix "accept_local=1") (
  lib.toList config.systemd.services.tailscaled.serviceConfig.ExecStartPost
);
assert lib.hasInfix "meta mark" config.networking.firewall.extraReversePathFilterRules;
# Every secret arrives as a systemd credential from the host-secrets mount;
# a missing source file keeps the unit from starting.
assert
  unit.serviceConfig.LoadCredential == [
    "ca-cert:/run/host-secrets/ca-cert"
    "ca-key:/run/host-secrets/ca-key"
    "op-connect-token:/run/host-secrets/op-connect-token"
  ];
assert minimal.config.systemd.services.ts-egress.serviceConfig.LoadCredential == [ ];
assert
  config.systemd.services.tailscaled-autoconnect.serviceConfig.LoadCredential == [
    "ts-authkey:/run/host-secrets/ts-authkey"
  ];
assert
  config.services.tailscale.authKeyFile
  == "/run/credentials/tailscaled-autoconnect.service/ts-authkey";
assert !(unit ? environment && unit.environment ? OP_CONNECT_HOST);

# The only flag is the generated config file.
assert
  builtins.match "/nix/store/[^ ]+/bin/ts-egress -config /nix/store/[^ ]+-ts-egress[.]json" exec
  != null;

# The config names credentials and holds no secret or secret path. ts-egress
# accepts it and, without the credentials, refuses to start (exit 1, not the
# usage exit 2).
pkgs.runCommand "ts-egress-evaluation"
  {
    nativeBuildInputs = [
      pkgs.jq
      config.modules.services.ts-egress.package
    ];
  }
  ''
    full=${configFile}
    minimal=${minimalConfigFile}
    jq -e '
      .tcpListen == ["0.0.0.0:15001", "[::]:15001"]
      and .udpListen == .tcpListen
      and .tailscaledSocket == "/run/tailscale/tailscaled.sock"
      and .dnsUpstream == "192.0.2.1:53"
      and .self == ["192.0.2.26"]
      and .helper == {listen: "192.0.2.26:3128"}
      and .ca == {certCredential: "ca-cert", keyCredential: "ca-key"}
      and .onepassword == {url: "http://op-connect.example.ts.net:8080", tokenCredential: "op-connect-token"}
      and .brokerSocket == "/run/ts-broker/decide.sock"
    ' "$full"
    jq -e '. == {
      tcpListen: ["0.0.0.0:15001", "[::]:15001"],
      udpListen: ["0.0.0.0:15001", "[::]:15001"],
      tailscaledSocket: "/run/tailscale/tailscaled.sock",
      dnsUpstream: "192.0.2.1:53",
      self: []
    }' "$minimal"
    for f in "$full" "$minimal"; do
      if grep -E 'host-secrets|/run/secrets|/run/credentials|BEGIN|PRIVATE' "$f"; then
        echo "$f carries a secret path or value" >&2
        exit 1
      fi
    done

    status=0
    env -u CREDENTIALS_DIRECTORY ts-egress -config "$full" 2>err || status=$?
    cat err
    test "$status" = 1
    grep -qF 'credential ca-cert: $CREDENTIALS_DIRECTORY is not set' err
    touch $out
  ''
