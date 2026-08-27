{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  evaluated = lib.nixosSystem {
    modules = [
      ../modules/networking/systemd-netns-private.nix
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.networking.systemd-netns-private = {
          enable = true;
          namespaces = {
            home-dl = {
              hostAddr = "192.168.10.8/29";
              nsAddr = "192.168.10.9/29";
              hostIface = "home-dl-host";
              nsIface = "home-dl-ns";
              egress = {
                source = "192.168.10.8/29";
                interface = "prim";
                onLinkSubnet = "10.9.4.0/22";
                gateway = "10.9.4.1";
                routingTable = 100;
              };
            };
            existing = {
              hostAddr = "192.168.20.1/30";
              nsAddr = "192.168.20.2/30";
            };
          };
        };
      }
    ];
  };
  homeDlUnit = evaluated.config.systemd.services.systemd-netns-private-access-home-dl;
  existingUnit = evaluated.config.systemd.services.systemd-netns-private-access-existing;
  homeDl = homeDlUnit.serviceConfig;
  existing = existingUnit.serviceConfig;
  homeDlCommands = homeDl.ExecStartPre ++ homeDl.ExecStart ++ homeDl.ExecStopPost;
  existingCommands = existing.ExecStartPre ++ existing.ExecStart ++ existing.ExecStopPost;
  ip = "${pkgs.iproute2}/bin/ip";
  iptables = "${pkgs.iptables}/bin/iptables";
in
assert builtins.elem "network-online.target" homeDlUnit.wants;
assert builtins.elem "network-online.target" homeDlUnit.after;
assert !(builtins.elem "network-online.target" existingUnit.wants);
assert !(builtins.elem "network-online.target" existingUnit.after);
assert builtins.elem "-${ip} rule del priority 100 from 192.168.10.8/29 table 100"
  homeDl.ExecStartPre;
assert builtins.elem "-${ip} route del 192.168.10.8/29 dev home-dl-host table 100"
  homeDl.ExecStartPre;
assert builtins.elem "-${ip} route del 10.9.4.0/22 dev prim table 100" homeDl.ExecStartPre;
assert builtins.elem "-${ip} route del default via 10.9.4.1 dev prim table 100" homeDl.ExecStartPre;
assert builtins.elem "-${iptables} -t nat -D POSTROUTING -s 192.168.10.8/29 -o prim -j MASQUERADE"
  homeDl.ExecStartPre;
assert builtins.elem
  "${ip} route replace 192.168.10.8/29 dev home-dl-host src 192.168.10.8 table 100"
  homeDl.ExecStart;
assert builtins.elem "${ip} route replace 10.9.4.0/22 dev prim table 100" homeDl.ExecStart;
assert builtins.elem "${ip} route replace default via 10.9.4.1 dev prim table 100" homeDl.ExecStart;
assert builtins.elem "${ip} rule add priority 100 from 192.168.10.8/29 table 100" homeDl.ExecStart;
assert builtins.elem "${iptables} -t nat -A POSTROUTING -s 192.168.10.8/29 -o prim -j MASQUERADE"
  homeDl.ExecStart;
assert builtins.elem "-${iptables} -t nat -D POSTROUTING -s 192.168.10.8/29 -o prim -j MASQUERADE"
  homeDl.ExecStopPost;
assert builtins.elem "-${ip} rule del priority 100 from 192.168.10.8/29 table 100"
  homeDl.ExecStopPost;
assert builtins.elem "-${ip} route del 192.168.10.8/29 dev home-dl-host table 100"
  homeDl.ExecStopPost;
assert builtins.elem "-${ip} route del 10.9.4.0/22 dev prim table 100" homeDl.ExecStopPost;
assert builtins.elem "-${ip} route del default via 10.9.4.1 dev prim table 100" homeDl.ExecStopPost;
assert builtins.elem "-${ip} netns exec home-dl ip route add default via 192.168.10.8"
  homeDl.ExecStart;
assert lib.all (command: !(lib.hasInfix " -o mgmt " command)) homeDlCommands;
assert builtins.elem "${iptables} -t nat -A POSTROUTING -s 192.168.20.2/30 -o mgmt -j MASQUERADE"
  existing.ExecStart;
assert lib.all (command: !(lib.hasInfix " table 100" command)) existingCommands;
pkgs.runCommand "systemd-netns-private-egress-evaluation" { } "touch $out"
