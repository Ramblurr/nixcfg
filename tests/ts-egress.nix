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
  inputRules = config.networking.firewall.extraInputRules;
  unit = config.systemd.services.ts-egress;
  exec = unit.serviceConfig.ExecStart;
  configFile = lib.last (lib.splitString " " exec);
  minimalExec = minimal.config.systemd.services.ts-egress.serviceConfig.ExecStart;
  minimalConfigFile = lib.last (lib.splitString " " minimalExec);
in
assert failedAssertions full == [ ];
assert failedAssertions minimal == [ ];
assert failedAssertions halfOnepassword != [ ];
assert failedAssertions halfBroker != [ ];
assert unit.serviceConfig.SupplementaryGroups == [ "ts-broker-clients" ];
assert config.users.groups ? ts-broker-clients;
assert minimal.config.systemd.services.ts-egress.serviceConfig.SupplementaryGroups == [ ];
assert !(builtins.tryEval storeKey.config.modules.services.ts-egress.ca.keyFile).success;

# The tsnet node is the exit node: no tailscaled, no TPROXY ruleset, no
# routing or reverse-path exceptions.
assert !config.services.tailscale.enable;
assert !(config.networking.nftables.tables ? ts-egress);
assert config.networking.firewall.extraReversePathFilterRules == "";
assert config.networking.firewall.allowedUDPPorts == [ 41641 ];

# Only the helper port from the LAN is let in, through the nftables firewall
# that extraInputRules needs.
assert config.networking.nftables.enable;
assert !minimal.config.networking.nftables.enable;
assert lib.hasInfix ''iifname "svc" ip saddr { 192.0.2.0/24 } tcp dport 3128 accept'' inputRules;
assert !(lib.hasInfix "3128" minimal.config.networking.firewall.extraInputRules);

# The service keeps its node in its own state directory and holds no
# capabilities.
assert unit.serviceConfig.StateDirectory == "ts-egress";
assert unit.serviceConfig.CapabilityBoundingSet == "";
assert !(unit.serviceConfig ? AmbientCapabilities);
assert unit.serviceConfig.Restart == "always";
# Every secret arrives as a systemd credential from the host-secrets mount;
# a missing source file keeps the unit from starting.
assert
  unit.serviceConfig.LoadCredential == [
    "ts-authkey:/run/host-secrets/ts-authkey"
    "ca-cert:/run/host-secrets/ca-cert"
    "ca-key:/run/host-secrets/ca-key"
    "op-connect-token:/run/host-secrets/op-connect-token"
  ];
assert
  minimal.config.systemd.services.ts-egress.serviceConfig.LoadCredential == [
    "ts-authkey:/run/host-secrets/ts-authkey"
  ];
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
      .hostname == "egress"
      and .port == 41641
      and .stateDir == "/var/lib/ts-egress"
      and .authKeyCredential == "ts-authkey"
      and .tags == ["tag:egress"]
      and .dnsUpstream == "192.0.2.1:53"
      and .self == ["192.0.2.26"]
      and .helper == {listen: "192.0.2.26:3128"}
      and .ca == {certCredential: "ca-cert", keyCredential: "ca-key"}
      and .onepassword == {url: "http://op-connect.example.ts.net:8080", tokenCredential: "op-connect-token"}
      and .brokerSocket == "/run/ts-broker/decide.sock"
    ' "$full"
    jq -e '. == {
      hostname: "egress",
      port: 41641,
      stateDir: "/var/lib/ts-egress",
      authKeyCredential: "ts-authkey",
      tags: ["tag:egress"],
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
    status=0
    env -u CREDENTIALS_DIRECTORY ts-egress -config "$minimal" 2>err || status=$?
    cat err
    test "$status" = 1
    grep -qF 'credential ts-authkey: $CREDENTIALS_DIRECTORY is not set' err
    touch $out
  ''
