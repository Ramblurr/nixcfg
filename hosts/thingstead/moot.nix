{ config, lib, ... }:
let
  ward = lib.head config.site.net.svc.hosts4.ward;
  steads = [
    "workstead"
    "homestead"
    "outstead"
  ];
in
{
  modules.microvm-host = {
    mootNodeUplinks = [
      "eno1"
      "data"
    ];
    mootNodePeers = [
      {
        address = ward;
        port = 41641;
      }
      {
        address = ward;
        port = 41642;
      }
    ];
    mootNodes = lib.listToAttrs (
      lib.imap0 (
        i: vm:
        lib.nameValuePair vm {
          tap = {
            id = "mw-${vm}";
            hostAddress = "10.77.0.1";
            guestAddress = "10.77.0.2";
          };
          uplink = {
            id = "mn-${vm}";
            hostAddress = "172.31.255.${toString (4 * i + 1)}";
            nodeAddress = "172.31.255.${toString (4 * i + 2)}";
          };
          authKeyFile = config.modules.services.onepassword-systemd-credentials.socketPath;
          dnsResolver = "1.1.1.1";
        }
      ) steads
    );
  };

  microvm.vms = lib.genAttrs steads (_: {
    autostart = true;
    restartIfChanged = true;
  });

  modules.services.onepassword-systemd-credentials = {
    enable = true;
    consumers = lib.listToAttrs (
      map (
        vm:
        lib.nameValuePair "moot-node@${vm}" {
          ts-authkey = "op://thingstead/thingstead-bootstrap/${vm}-authkey";
        }
      ) steads
    );
  };
}
