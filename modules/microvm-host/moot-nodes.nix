guestConfig:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-host;
  nodes = cfg.mootNodes;
  ipv4 = import ../../lib/ipv4.nix { inherit lib; };
  nonPublic = import ../../config/non-public-ipv4.nix;
  nftSet = values: "{ ${lib.concatStringsSep ", " values} }";
  uplinkIds = ''"mn-*"'';
  uplinks = nftSet (map (iface: ''"${iface}"'') cfg.mootNodeUplinks);
  ip = lib.getExe' pkgs.iproute2 "ip";
  transparentAddress = "127.0.0.1:15001";
  netnsUnit = vm: "moot-node-netns@${vm}.service";
  etcFile = vm: suffix: "moot-nodes/${vm}.${suffix}";

  linkOption =
    description:
    lib.mkOption {
      inherit (ipv4) type;
      inherit description;
    };
  prefixOption = lib.mkOption {
    type = lib.types.ints.between 1 32;
    default = 30;
    description = "Prefix length of the link.";
  };

  linkFailures =
    vm: node:
    let
      tap = ipv4.subnet node.tap.hostAddress node.tap.prefixLength;
      uplink = ipv4.subnet node.uplink.hostAddress node.uplink.prefixLength;
      scope = "modules.microvm-host.mootNodes.${vm}";
    in
    lib.optional (!ipv4.isLink node.tap.hostAddress node.tap.guestAddress node.tap.prefixLength)
      "${scope}.tap needs hostAddress and guestAddress to be distinct host addresses of one /${toString node.tap.prefixLength}."
    ++
      lib.optional (!ipv4.isLink node.uplink.hostAddress node.uplink.nodeAddress node.uplink.prefixLength)
        "${scope}.uplink needs hostAddress and nodeAddress to be distinct host addresses of one /${toString node.uplink.prefixLength}."
    ++ lib.optional (ipv4.overlaps tap uplink) "${scope} uplink overlaps its tap subnet.";

  uplinkIdList = lib.mapAttrsToList (_: node: node.uplink.id) nodes;
  duplicateUplinkIds = lib.filter (id: lib.count (x: x == id) uplinkIdList > 1) (
    lib.unique uplinkIdList
  );
  uplinkSubnets = lib.mapAttrsToList (vm: node: {
    inherit vm;
    subnet = ipv4.subnet node.uplink.hostAddress node.uplink.prefixLength;
  }) nodes;
  overlappingUplinks = lib.concatLists (
    lib.imap0 (
      i: a:
      map (b: "${a.vm}/${b.vm}") (
        lib.filter (b: ipv4.overlaps a.subnet b.subnet) (lib.drop (i + 1) uplinkSubnets)
      )
    ) uplinkSubnets
  );

  wright = vm: (guestConfig vm config.microvm.vms.${vm}).modules.microvm-guest.mootWright or { };
  isWright = vm: (wright vm).enable or false;
  guestFailures =
    vm: node:
    let
      guest = (wright vm).network;
      shared = {
        "tap.id" = [
          node.tap.id
          guest.tapId
        ];
        "tap.hostAddress" = [
          node.tap.hostAddress
          guest.hostAddress
        ];
        "tap.guestAddress" = [
          node.tap.guestAddress
          guest.guestAddress
        ];
        "tap.prefixLength" = [
          node.tap.prefixLength
          guest.prefixLength
        ];
        dnsResolver = [
          node.dnsResolver
          guest.dnsResolver
        ];
        t3Port = [
          node.t3Port
          guest.t3Port
        ];
        proxyPort = [
          node.proxyPort
          guest.proxyPort
        ];
      };
    in
    if !(isWright vm) then
      [ "modules.microvm-host.mootNodes.${vm} names a MicroVM that is not a Moot wright." ]
    else
      lib.concatLists (
        lib.mapAttrsToList (
          field: values:
          lib.optional (lib.head values != lib.last values)
            "modules.microvm-host.mootNodes.${vm} disagrees with its guest on ${field}: ${toString (lib.head values)} here, ${toString (lib.last values)} in the guest."
        ) shared
      );

  failures =
    lib.optional (nodes != { } && cfg.mootNodeUplinks == [ ])
      "modules.microvm-host.mootNodeUplinks must name at least one uplink interface for modules.microvm-host.mootNodes."
    ++ lib.optional (
      duplicateUplinkIds != [ ]
    ) "modules.microvm-host.mootNodes share uplink ids: ${lib.concatStringsSep ", " duplicateUplinkIds}"
    ++
      lib.optional (overlappingUplinks != [ ])
        "modules.microvm-host.mootNodes have overlapping uplink subnets: ${lib.concatStringsSep ", " overlappingUplinks}"
    ++ lib.concatLists (lib.mapAttrsToList linkFailures nodes)
    ++ lib.concatLists (
      lib.mapAttrsToList guestFailures (lib.filterAttrs (vm: _: config.microvm.vms ? ${vm}) nodes)
    )
    ++ map (vm: "Moot wright MicroVM ${vm} has no modules.microvm-host.mootNodes record.") (
      lib.filter (vm: isWright vm && !(nodes ? ${vm})) (builtins.attrNames config.microvm.vms)
    );

  setupNamespace = pkgs.writeShellScript "moot-node-netns" ''
    set -euo pipefail
    ns="moot-$1"
    in_ns() { ip netns exec "$ns" "$@"; }

    ip netns delete "$ns" 2>/dev/null || true
    ip link delete "$MOOT_UPLINK" 2>/dev/null || true
    ip netns add "$ns"
    in_ns sysctl -qw net.ipv6.conf.all.disable_ipv6=1 net.ipv6.conf.default.disable_ipv6=1 net.ipv4.ip_nonlocal_bind=1
    in_ns nft -f - <<EOF
    table inet moot-node {
      chain prerouting {
        type filter hook prerouting priority mangle; policy accept;
        iifname != "$MOOT_TAP" return
        meta nfproto != ipv4 counter drop
        ip saddr != $MOOT_GUEST counter drop
        ct direction reply ip daddr $MOOT_TAP_HOST accept
        ip daddr $MOOT_TAP_HOST tcp dport $MOOT_PROXY_PORT accept
        meta l4proto { tcp, udp } tproxy ip to ${transparentAddress} meta mark set 1 accept
        counter drop
      }
      chain forward {
        type filter hook forward priority filter; policy drop;
      }
    }
    EOF
    in_ns ip link set lo up
    in_ns ip rule add fwmark 1 lookup 100
    in_ns ip route add local 0.0.0.0/0 dev lo table 100

    ip link add "$MOOT_UPLINK" type veth peer name uplink netns "$ns"
    ip addr add "$MOOT_UPLINK_HOST/$MOOT_UPLINK_PREFIX" dev "$MOOT_UPLINK"
    ip link set "$MOOT_UPLINK" up
    in_ns ip addr add "$MOOT_UPLINK_NODE/$MOOT_UPLINK_PREFIX" dev uplink
    in_ns ip link set uplink up
    in_ns ip route add default via "$MOOT_UPLINK_HOST"
  '';

  startNode = pkgs.writeShellScript "moot-node" ''
    TS_AUTHKEY="$(< "$CREDENTIALS_DIRECTORY/ts-authkey")"
    export TS_AUTHKEY
    exec ${lib.getExe pkgs.ts-plug} \
      -hostname "$MOOT_HOSTNAME" \
      -dir "$STATE_DIRECTORY" \
      -https-port "443:$MOOT_GUEST:$MOOT_T3_PORT" \
      -response-header-timeout 0 \
      -http-proxy "$MOOT_TAP_HOST:$MOOT_PROXY_PORT" \
      -exit-node "$MOOT_EXIT_NODE" \
      -dns-resolver "$MOOT_DNS_RESOLVER:53" \
      -transparent-tcp ${transparentAddress} \
      -transparent-udp ${transparentAddress} \
      ''${MOOT_ADVERTISE_TAGS:+-advertise-tags "$MOOT_ADVERTISE_TAGS"}
  '';

  inNamespace = vm: {
    overrideStrategy = "asDropin";
    bindsTo = [ (netnsUnit vm) ];
    after = [ (netnsUnit vm) ];
    serviceConfig.NetworkNamespacePath = "/run/netns/moot-${vm}";
  };
in
{
  options.modules.microvm-host = {
    mootNodeUplinks = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Host interfaces through which Moot tailnet nodes reach the public internet.";
    };
    mootNodes = lib.mkOption {
      default = { };
      description = ''
        The tailnet node of each Moot agent MicroVM, keyed by VM name. The
        host runs the node in a network namespace that also holds the VM's
        tap, so everything the VM sends reaches only the node.
      '';
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              tap = {
                id = lib.mkOption {
                  type = lib.types.strMatching "[a-zA-Z0-9-]{1,15}";
                  description = "Tap interface name; the guest's microvm.interfaces id.";
                };
                hostAddress = linkOption "The node's IPv4 address on the tap, the guest's gateway.";
                guestAddress = linkOption "The guest's IPv4 address on the tap.";
                prefixLength = prefixOption;
              };
              uplink = {
                id = lib.mkOption {
                  type = lib.types.strMatching "mn-[a-z0-9-]{1,12}";
                  description = "Host-side name of the veth that carries the node's own traffic.";
                };
                hostAddress = linkOption "The host's IPv4 address on the uplink, the node's gateway.";
                nodeAddress = linkOption "The node's IPv4 address on the uplink.";
                prefixLength = prefixOption;
              };
              hostname = lib.mkOption {
                type = lib.types.strMatching "[a-zA-Z0-9-]+";
                default = name;
                defaultText = lib.literalExpression "<name>";
                description = "Tailnet hostname of the node.";
              };
              advertiseTags = lib.mkOption {
                type = lib.types.listOf (lib.types.strMatching "tag:[a-zA-Z0-9-]+");
                default = [ ];
                description = ''
                  ACL tags the node requests. Required when the auth key file
                  holds an OAuth client secret (tskey-client-...); leave empty
                  for a pre-tagged auth key (tskey-auth-...).
                '';
              };
              authKeyFile = lib.mkOption {
                type = lib.types.strMatching "/.+";
                description = "Host path of the node's Tailscale auth key or OAuth client secret.";
              };
              exitNode = lib.mkOption {
                type = lib.types.strMatching "[a-zA-Z0-9.:-]+";
                default = "ward";
                description = "Exit node carrying the VM's traffic outside the tailnet.";
              };
              dnsResolver = lib.mkOption {
                inherit (ipv4) type;
                description = "Public numeric DNS server the guest uses and the node queries through the exit node.";
              };
              t3Port = lib.mkOption {
                type = lib.types.port;
                default = 3780;
                description = "Guest port that the node's tailnet HTTPS on 443 proxies to.";
              };
              proxyPort = lib.mkOption {
                type = lib.types.port;
                default = 1056;
                description = "Port of the node's HTTP proxy on the tap.";
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        assertions = map (message: {
          assertion = false;
          inherit message;
        }) failures;
      }
      (lib.mkIf (nodes != { }) {
        boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;

        environment.etc = lib.concatMapAttrs (vm: node: {
          ${etcFile vm "env"}.text = ''
            MOOT_TAP=${node.tap.id}
            MOOT_TAP_HOST=${node.tap.hostAddress}
            MOOT_GUEST=${node.tap.guestAddress}
            MOOT_UPLINK=${node.uplink.id}
            MOOT_UPLINK_HOST=${node.uplink.hostAddress}
            MOOT_UPLINK_NODE=${node.uplink.nodeAddress}
            MOOT_UPLINK_PREFIX=${toString node.uplink.prefixLength}
            MOOT_HOSTNAME=${node.hostname}
            MOOT_ADVERTISE_TAGS=${lib.concatStringsSep "," node.advertiseTags}
            MOOT_EXIT_NODE=${node.exitNode}
            MOOT_DNS_RESOLVER=${node.dnsResolver}
            MOOT_T3_PORT=${toString node.t3Port}
            MOOT_PROXY_PORT=${toString node.proxyPort}
          '';
          ${etcFile vm "resolv.conf"}.text = ''
            nameserver ${node.dnsResolver}
          '';
        }) nodes;

        # systemd refuses a DynamicUser state directory under a
        # world-readable /var/lib/private. Each activation, impermanence
        # copies the persisted parent's mode onto it, so fix both.
        systemd.tmpfiles.rules = [
          "d /persist/var/lib/private 0700 root root -"
          "d /var/lib/private 0700 root root -"
        ];
        environment.persistence."/persist".directories = [ "/var/lib/private/moot-nodes" ];

        systemd.services = {
          "moot-node-netns@" = {
            description = "Network namespace of Moot node %i";
            # nftables.service deletes its tables on stop, which would leave
            # the uplink unfiltered; reloads keep them in place.
            bindsTo = [ "nftables.service" ];
            after = [
              "nftables.service"
              "network.target"
            ];
            path = [
              pkgs.iproute2
              pkgs.nftables
              pkgs.procps
            ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              EnvironmentFile = "/etc/${etcFile "%i" "env"}";
              ExecStart = "${setupNamespace} %i";
              ExecStopPost = [
                "-${ip} netns delete moot-%i"
                "-${ip} link delete $MOOT_UPLINK"
              ];
            };
          };
          "moot-node@" = {
            description = "Tailnet node of Moot MicroVM %i";
            bindsTo = [ "moot-node-netns@%i.service" ];
            after = [ "moot-node-netns@%i.service" ];
            environment.TS_NO_LOGS_NO_SUPPORT = "true";
            serviceConfig = {
              NetworkNamespacePath = "/run/netns/moot-%i";
              EnvironmentFile = "/etc/${etcFile "%i" "env"}";
              BindReadOnlyPaths = [ "/etc/${etcFile "%i" "resolv.conf"}:/etc/resolv.conf" ];
              DynamicUser = true;
              User = "moot-node-%i";
              StateDirectory = "moot-nodes/%i";
              StateDirectoryMode = "0700";
              ExecStart = startNode;
              # IP_TRANSPARENT needs CAP_NET_RAW, and UDP replies leave from
              # the original destination port, such as 53.
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
        }
        // lib.concatMapAttrs (vm: node: {
          "microvm@${vm}" = inNamespace vm // {
            wants = [ "moot-node@${vm}.service" ];
          };
          "microvm-tap-interfaces@${vm}" = lib.recursiveUpdate (inNamespace vm) {
            serviceConfig.ExecStartPost = "${ip} addr replace ${node.tap.hostAddress}/${toString node.tap.prefixLength} dev ${node.tap.id}";
          };
          "moot-node@${vm}" = {
            overrideStrategy = "asDropin";
            serviceConfig.LoadCredential = [ "ts-authkey:${node.authKeyFile}" ];
          };
        }) nodes;

        networking.nftables.enable = true;
        networking.nftables.tables.moot-nodes = {
          family = "inet";
          content = ''
            chain input {
              type filter hook input priority filter; policy accept;
              iifname ${uplinkIds} counter drop
            }
            chain forward {
              type filter hook forward priority filter; policy accept;
              iifname ${uplinkIds} meta nfproto ipv6 counter drop
              iifname ${uplinkIds} ip daddr ${nftSet nonPublic} counter drop
              iifname ${uplinkIds} oifname ${uplinks} accept
              iifname ${uplinkIds} counter drop
              oifname ${uplinkIds} ct state { established, related } accept
              oifname ${uplinkIds} counter drop
            }
            chain postrouting {
              type nat hook postrouting priority srcnat; policy accept;
              iifname ${uplinkIds} oifname ${uplinks} masquerade
            }
          '';
        };
      })
    ]
  );
}
