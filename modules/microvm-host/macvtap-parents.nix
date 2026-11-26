guestConfig:
{
  config,
  lib,
  utils,
  ...
}:
let
  parentLinks =
    name: vm:
    lib.unique (
      map (interface: interface.macvtap.link) (
        builtins.filter (interface: interface.type == "macvtap") (guestConfig name vm).microvm.interfaces
      )
    );
  linkDevice = link: "sys-subsystem-net-devices-${utils.escapeSystemdPath link}.device";
in
{
  config = lib.mkIf config.modules.microvm-host.enable {
    systemd.services = lib.mapAttrs' (
      name: links:
      lib.nameValuePair "microvm-macvtap-interfaces@${name}" {
        overrideStrategy = "asDropin";
        requisite = map linkDevice links;
        after = map linkDevice links;
      }
    ) (lib.filterAttrs (_: links: links != [ ]) (lib.mapAttrs parentLinks config.microvm.vms));
  };
}
