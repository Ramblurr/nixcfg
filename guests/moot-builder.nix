{
  vcpu,
  mem,
  zvolSize,
}:
{ config, ... }:
let
  inherit (config.repo.secrets) global;
in
{
  system.stateVersion = "26.11";

  modules.microvm-guest = {
    host = "mootforge";
    hostFQDN = "mootforge.${config.site.net.mgmt.domainName}";
    mootBuilder.enable = true;
    mootVm = {
      inherit zvolSize;
      network = {
        tapId = "mb-${config.networking.hostName}";
        hostAddress = "10.78.0.1";
        guestAddress = "10.78.0.2";
      };
      nix = {
        substituters = [ "https://moot-cache.${global.domain.tailnet}" ];
        trustedPublicKeys = [ global.nixCachePublicKey ];
      };
    };
  };
  microvm = { inherit vcpu mem; };
}
