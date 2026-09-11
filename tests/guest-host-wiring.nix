{
  configurations,
  guestHosts,
  lib,
  pkgs,
}:
assert lib.assertMsg (lib.all
  (
    guestName:
    let
      hostName = guestHosts.${guestName};
      guest = configurations.${guestName};
      attachedGuest = configurations.${hostName}.config.microvm.vms.${guestName};
    in
    attachedGuest.evaluatedConfig.config.networking.hostName == guest.config.networking.hostName
  )
  (builtins.attrNames guestHosts)
) "Every public guest placement must use its secret-aware guest output";
pkgs.runCommand "check-guest-host-wiring" { } "touch $out"
