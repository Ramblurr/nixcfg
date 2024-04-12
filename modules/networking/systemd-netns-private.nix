{
  config,
  lib,
  pkgs,
  utils,
  ...
}:

# This NixOS module configures a private network namespace with a veth-pair,
# facilitating isolated communication between a systemd service running within
# the namespace and the host system. By creating a dedicated network namespace,
# and establishing a veth-pair connection, the module ensures that both the host
# and the service within the ns can communicate over a private subnet.
#
# This setup is designed to enhance security and reduce potential network
# conflicts, allowing the service to operate with a degree of isolation from the
# host's main networking environment, yet still enabling controlled connectivity
# between the service and the host, as well as allowing the namespace-configured
# service to initiate outbound connections as necessary.
#
# inspired by: https://www.cloudnull.io/2019/04/running-services-in-network-name-spaces-with-systemd/
#
#    modules.networking.systemd-netns-private = {
#      enable = true;
#      namespaces.my-ns = {
#        hostAddr = "192.168.10.1/24";
#        nsAddr = "192.168.10.2/24";
#        services = [ "my-service.service" ];
#      };
#    };
#    systemd.services.my-service { ... as usual .... };
#
# This will create a new networknamespace `my-ns`, modify `my-service.service` to run in that ns.
# ref: https://github.com/systemd/systemd/issues/28694
let
  cfg = config.modules.networking.systemd-netns-private;
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
  extractName =
    s:
    let
      prefix = lib.concatStringsSep "." (lib.init (lib.splitString "." s));
    in
    assert prefix != "";
    prefix;
in
{
  options = {
    modules.networking.systemd-netns-private = {
      enable = lib.mkEnableOption "Enable systemd-netns-private";

      namespaces = lib.mkOption {
        description = "List of private networking namespaces to create";
        default = { };
        type = lib.types.attrsOf (
          lib.types.submodule (
            { name, ... }:
            {
              options = {
                hostAddr = lib.mkOption {
                  type = lib.types.str;
                  description = "CIDR of the host ip address";
                };
                nsAddr = lib.mkOption {
                  type = lib.types.str;
                  description = "CIDR of the ip address in the namespace";
                };
                services = lib.mkOption {
                  type = lib.types.listOf utils.systemdUtils.lib.unitNameType;
                  description = lib.mdDoc "Names of `systemd.services.<name>` to override to use the namespace";
                  default = [ ];
                };
                hostIface = lib.mkOption {
                  type = lib.types.str;
                  description = "iface name for the host. Defaults to veth-<name>-host";
                };
                nsIface = lib.mkOption {
                  type = lib.types.str;
                  description = "iface name for the namespace side. Defaults to veth-<name>-ns.";
                };
                name = lib.mkOption {
                  type = lib.types.strMatching "[a-zA-Z0-9@%:_.\\-]+";
                  description = "The namespace name. Defaults to <name>";
                };
              };
              config = {
                hostIface = lib.mkDefault "veth-${name}-host";
                nsIface = lib.mkDefault "veth-${name}-ns";
                name = lib.mkDefault name;
              };
            }
          )
        );
      };
    };
  };
  config = lib.mkIf cfg.enable {
    assertions =
      let
        keepTooLong = list: lib.filter (s: lib.stringLength s > 15) list;
        hostIfaces = keepTooLong (lib.mapAttrsToList (name: ns: ns.hostIface) cfg.namespaces);
        nsIfaces = keepTooLong (lib.mapAttrsToList (name: ns: ns.nsIface) cfg.namespaces);
        ifaces = hostIfaces ++ nsIfaces;
      in
      [
        {
          assertion = lib.length ifaces == 0;
          message = "interface names must be 15 characters or less: ${lib.concatStringsSep ", " ifaces}";
        }
      ];
    systemd.tmpfiles.rules =
      let
        nsNames = lib.mapAttrsToList (name: ns: ns.name) cfg.namespaces;
      in
      lib.flatten (
        map (ns: [
          "d /etc/netns/${ns} 0700 root root -"
          "L+ /etc/netns/${ns}/resolv.conf - - - - /etc/resolv-external.conf"
        ]) nsNames
      );
    systemd.services =
      let
        mkOverrides =
          name: ns:
          map (serviceName: {
            name = extractName serviceName;
            value = {
              bindsTo = [ "systemd-netns-private-${name}.service" ];
              after = [ "systemd-netns-private-${name}.service" ];
              serviceConfig = {
                NetworkNamespacePath = "/var/run/netns/${ns.name}";
                PrivateNetwork = true;
                Slice = "${name}.slice";
                CPUAccounting = true;
                BlockIOAccounting = true;
                MemoryAccounting = true;
                TasksAccounting = true;
                BindReadOnlyPaths = [ "/etc/netns/${ns.name}/resolv.conf:/etc/resolv.conf:norbind" ];
              };
            };
          }) ns.services;
        mkServices = name: ns: [
          {
            name = "systemd-netns-private-${name}";
            value = {
              description = "Creates network namespace for ${name}";
              after = [ "network.target" ];
              partOf = ns.services;
              bindsTo = [ "systemd-netns-private-access-${name}.service" ];

              unitConfig = {
                JoinsNamespaceOf = [ "systemd-netns-private-${name}" ];
              };
              path = [
                pkgs.mount
                pkgs.iproute2
                pkgs.procps
              ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                PrivateNetwork = true;
                ExecStartPre = "-${pkgs.iproute}/bin/ip netns delete ${ns.name}";
                ExecStart = [
                  "${pkgs.iproute}/bin/ip netns add ${ns.name}"
                  "${pkgs.iproute}/bin/ip netns exec ${ns.name} ${pkgs.iproute}/bin/ip link set lo up"
                  "${pkgs.umount}/bin/umount /var/run/netns/${ns.name}"
                  "${pkgs.mount}/bin/mount --bind /proc/self/ns/net /var/run/netns/${ns.name}"
                ];
                ExecStopPost = "${pkgs.iproute2}/bin/ip netns delete ${ns.name}";
                # This is required since systemd commit c2da3bf, shipped in systemd 254.
                # See discussion at https://github.com/systemd/systemd/issues/28686
                PrivateMounts = false;
              };
            };
          }
          {
            name = "systemd-netns-private-access-${name}";
            value = {
              description = "Creates network namespace access resources for ${name}";
              after = [
                "network.target"
                "systemd-netns-private-${name}.service"
              ];
              before = ns.services;
              bindsTo = [ "systemd-netns-private-${name}.service" ];
              path = [
                pkgs.mount
                pkgs.umount
                pkgs.iproute2
                pkgs.procps
              ];

              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                ExecStartPre = [
                  # Create system process
                  "-${pkgs.iproute}/bin/ip link add ${ns.hostIface} type veth peer name ${ns.nsIface}"
                  "-${pkgs.iproute}/bin/ip addr add ${ns.hostAddr} dev ${ns.hostIface}"
                  "-${pkgs.iproute}/bin/ip link set ${ns.hostIface} up"
                  "-${pkgs.procps}/bin/sysctl -w net.ipv4.ip_forward=1"
                ];

                ExecStart = [
                  "${pkgs.iproute}/bin/ip link set ${ns.nsIface} netns ${ns.name}"
                  "${pkgs.iproute}/bin/ip netns exec ${ns.name} ip addr add ${ns.nsAddr} dev ${ns.nsIface}"
                  "${pkgs.iproute}/bin/ip netns exec ${ns.name} ip link set ${ns.nsIface} up"
                  "-${pkgs.iproute}/bin/ip netns exec ${ns.name} ip link set lo up"
                  "-${pkgs.iproute}/bin/ip netns exec ${ns.name} ip route add default via ${cidrToIp ns.hostAddr}"
                  "${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${ns.nsAddr} -o brmgmt9 -j MASQUERADE"
                ];
                ExecStopPost = [
                  "${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${ns.nsAddr} -o brmgmt9 -j MASQUERADE"
                  "${pkgs.iproute2}/bin/ip link del ${ns.nsIface}"
                  "${pkgs.iproute2}/bin/ip link del ${ns.hostIface}"
                ];
              };
            };
          }
        ];

        # First, use mapAttrsToList to transform and collect results
        # into a list of lists of pairs
        listOfLists = lib.mapAttrsToList (
          name: value: ((mkServices name value) ++ (mkOverrides name value))
        ) cfg.namespaces;

        # Flatten the list of lists into a single list
        flattenedList = lib.flatten listOfLists;

        # Convert the list of pairs back into an attribute set
        combinedServices = builtins.listToAttrs flattenedList;
      in
      combinedServices;
  };
}
