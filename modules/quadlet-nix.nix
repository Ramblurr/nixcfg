# The upstream quadlet-nix NixOS module with this repository's defaults.
# Hosts, guests and tests import `import <path>/modules/quadlet-nix.nix inputs`
# instead of the upstream module, so they all evaluate the same Quadlet setup.
inputs:
{ lib, ... }:
{
  imports = [ inputs.quadlet-nix2.nixosModules.default ];

  # quadlet-nix reserves the upper half of the 32-bit IDs for a `containers`
  # user, from which rootful `UserNS=auto` containers take their ranges. Our
  # Quadlets are rootless and draw on their owners' subordinate IDs instead.
  virtualisation.quadlet.containersUser.enable = lib.mkDefault false;
}
