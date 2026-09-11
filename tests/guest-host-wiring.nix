{
  configurations,
  guestHosts,
  lib,
  pkgs,
}:
let
  placements = builtins.attrNames guestHosts;
  placementUsesSecretAwareOutput =
    guestName:
    let
      hostName = guestHosts.${guestName};
      guest = configurations.${guestName};
      attachedGuest = configurations.${hostName}.config.microvm.vms.${guestName};
    in
    attachedGuest.evaluatedConfig.config.networking.hostName == guest.config.networking.hostName
    && guest.config.modules.microvm-guest.host == hostName;
  placementHasMacvtapParents =
    guestName:
    let
      hostName = guestHosts.${guestName};
      host = configurations.${hostName}.config;
      attachedGuest = host.microvm.vms.${guestName};
      macvtapParents = map (interface: interface.macvtap.link) (
        builtins.filter (
          interface: interface.type == "macvtap"
        ) attachedGuest.evaluatedConfig.config.microvm.interfaces
      );
      hostNetdevNames = lib.mapAttrsToList (
        _: netdev: netdev.netdevConfig.Name
      ) host.systemd.network.netdevs;
    in
    lib.all (parent: builtins.elem parent hostNetdevNames) macvtapParents;
in
assert lib.assertMsg (lib.all placementUsesSecretAwareOutput placements)
  "Every public guest placement must target the guest-declared host and use its secret-aware output";
assert lib.assertMsg (lib.all placementHasMacvtapParents placements)
  "Every guest MACVTAP parent must be generated on its host";
pkgs.runCommand "check-guest-host-wiring" { } "touch $out"
