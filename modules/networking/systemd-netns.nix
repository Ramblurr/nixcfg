{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.networking.systemd-netns;
in
{
  options = {
    modules.networking.systemd-netns = {
      enable = lib.mkEnableOption "Enable systemd-netns";
    };
  };

  # services will need the following overrides
  # [Unit]
  # BindsTo = systemd-netns@ocis.service
  # After = systemd-netns@ocis.service
  #
  # [Service]
  # NetworkNamespacePath=/var/run/netns/ocis
  # PrivateNetwork = true
  # Slice = ocis.slice
  # ReadWritePaths=/tmp
  # CPUAccounting = true
  # BlockIOAccounting = true
  # MemoryAccounting = true
  # TasksAccounting = true

  config = lib.mkIf cfg.enable {
    # This is based on https://www.cloudnull.io/2019/04/running-services-in-network-name-spaces-with-systemd/
    systemd.services."systemd-netns@" = {
      description = "Named network namespace %i";
      after = [
        "network.target"
        "systemd-netns@%i.service"
      ];
      bindsTo = [ "systemd-netns-access@%i.service" ];
      partOf = [ "%i.service" ];
      #wantedBy = [
      #  "multi-user.target"
      #  "network-online.target"
      #];

      unitConfig = {
        JoinsNamespaceOf = [ "systemd-netns@%i.service" ];
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
        ExecStartPre = "-${pkgs.iproute}/bin/ip netns delete %I";
        ExecStart = [
          "${pkgs.iproute}/bin/ip netns add %I"
          "${pkgs.iproute}/bin/ip netns exec %I ${pkgs.iproute}/bin/ip link set lo up"
          "${pkgs.umount}/bin/umount /var/run/netns/%I"
          "${pkgs.mount}/bin/mount --bind /proc/self/ns/net /var/run/netns/%I"
        ];
        ExecStopPost = "${pkgs.iproute2}/bin/ip netns delete %I";
        # This is required since systemd commit c2da3bf, shipped in systemd 254.
        # See discussion at https://github.com/systemd/systemd/issues/28686
        PrivateMounts = false;
      };
    };
    systemd.services."systemd-netns-access@" = {
      description = "Named network namespace %I";
      after = [
        "network.target"
        "systemd-netns@%i.service"
      ];
      before = [ "%i.service" ];
      bindsTo = [ "systemd-netns@%i.service" ];
      #wantedBy = [
      #  "multi-user.target"
      #  "network-online.target"
      #];
      path = [
        pkgs.mount
        pkgs.umount
        pkgs.iproute2
        pkgs.procps
      ];
      serviceConfig =
        let

          runtimeScript = pkgs.writeShellScript "netns-runtime-script" ''
            set -ex
            specifier="$1"
            echo "netns-helper $specifier $2"
            function start {
              ${pkgs.iproute2}/bin/ip netns exec $specifier sysctl -w net.ipv4.conf.mv0.forwarding=1
              ${pkgs.iproute2}/bin/ip netns exec $specifier sysctl -w net.ipv4.conf.mv0.arp_notify=1
              ${pkgs.iproute2}/bin/ip netns exec $specifier sysctl -w net.ipv4.conf.mv0.arp_announce=2
              ${pkgs.iproute2}/bin/ip netns exec $specifier ip address add 10.9.8.100/23 dev mv0
              ${pkgs.iproute2}/bin/ip netns exec $specifier ip address add 10.9.8.101/23 dev mv0
              ${pkgs.iproute2}/bin/ip route add 10.9.8.100/32 dev mv-int metric 100 table local
              ${pkgs.iproute2}/bin/ip route add 10.9.8.101/32 dev mv-int metric 100 table local
            }


            function stop {
              ${pkgs.iproute2}/bin/ip route del 10.9.8.100/32 dev mv-int metric 100 table local
              ${pkgs.iproute2}/bin/ip route del 10.9.8.101/32 dev mv-int metric 100 table local
            }

            case "$2" in
               start)
                  start
               ;;
               stop)
                  stop
               ;;
               restart)
                  stop
                  start
               ;;
               *)
                  echo "Usage: $0 {start|stop|restart}"
            esac
          '';
        in
        {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStartPre = [
            # Create system process
            "-${pkgs.iproute}/bin/ip link add mv-int link brmgmt9 type macvlan mode bridge"
            "-${pkgs.iproute}/bin/ip link set mv-int up"
            "-${pkgs.procps}/bin/sysctl -w net.ipv4.ip_forward=1"
          ];
          # TODO host side link shim so host can talk to netns
          # ip link add ocis-shim link brmgmt9 type macvlan mode bridge
          # ip addr add 10.9.8.102/32 dev foobar-shim
          # ip link set foobar-shim up
          # ip route add 10.9.8.100/32 dev foobar-shim
          # ip route del 10.9.8.100/32 dev mv-int metric 100 table local
          # ip route del 10.9.8.101/32 dev mv-int metric 100 table local

          ExecStart = [
            # Pivot link
            "${pkgs.iproute}/bin/ip link add mv0 link mv-int type macvlan mode bridge"
            "${pkgs.iproute}/bin/ip link set mv0 netns %i name mv0"
            # Configure link
            "-${pkgs.iproute}/bin/ip netns exec %i ip link set lo up"
            "-${pkgs.iproute}/bin/ip netns exec %i ip link set dev mv0 up"
            "-${runtimeScript} %i start"
          ];

          ExecStop = [ "${runtimeScript} %i stop" ];
          ExecStopPost = [
            "${pkgs.iproute2}/bin/ip link del mv0"
            "${pkgs.iproute2}/bin/ip link del mv-int"
          ];
        };
    };
  };
}
