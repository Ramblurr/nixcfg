{ config, lib, ... }:
let
  ward = lib.head config.site.net.svc.hosts4.ward;
  inherit (config.site.net.data) hosts4;
  keys = "/persist/etc/moot-nodes";
  builders = {
    anvil = {
      index = 0;
      allowTags = [ "tag:moot-workstead" ];
    };
    cinder = {
      index = 1;
      allowTags = [ "tag:moot-outstead" ];
    };
  };
in
{
  modules.microvm-host = {
    mootNodeUplinks = [
      "mgmt"
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
    mootNodes = lib.mapAttrs (vm: builder: {
      tap = {
        id = "mb-${vm}";
        hostAddress = "10.78.0.1";
        guestAddress = "10.78.0.2";
      };
      uplink = {
        id = "mn-${vm}";
        hostAddress = "172.31.255.${toString (4 * builder.index + 1)}";
        nodeAddress = "172.31.255.${toString (4 * builder.index + 2)}";
      };
      endpoint = {
        address = lib.head hosts4.mootforge;
        port = 41650 + builder.index;
        allowFrom = hosts4.thingstead;
      };
      builder = {
        inherit (builder) allowTags;
        hostKeyFile = "${keys}/${vm}-ssh-host-key";
      };
      authKeyFile = "${keys}/${vm}-authkey";
      dnsResolver = "1.1.1.1";
    }) builders;
  };

  microvm.vms = lib.mapAttrs (_: _: {
    autostart = true;
    restartIfChanged = true;
  }) builders;
}
