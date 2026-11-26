guestConfig:
{
  config,
  lib,
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
in
{
  config = lib.mkIf config.modules.microvm-host.enable {
    systemd.services = lib.mapAttrs' (
      name: links:
      lib.nameValuePair "microvm-macvtap-interfaces@${name}" {
        overrideStrategy = "asDropin";
        preStart = ''
          for link in ${lib.escapeShellArgs links}; do
            if [ ! -e "/sys/class/net/$link" ]; then
              echo "MACVTAP parent link $link does not exist" >&2
              exit 1
            fi
          done
        '';
      }
    ) (lib.filterAttrs (_: links: links != [ ]) (lib.mapAttrs parentLinks config.microvm.vms));
  };
}
