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
  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "thingstead";
    hostFQDN = "thingstead.${config.site.net.moot.domainName}";
    mootWright.enable = true;
    mootVm = {
      inherit zvolSize;
      network = {
        tapId = "mw-${config.networking.hostName}";
        hostAddress = "10.77.0.1";
        guestAddress = "10.77.0.2";
      };
      nix = {
        substituters = [ "https://moot-cache.${global.domain.tailnet}" ];
        trustedPublicKeys = [ global.nixCachePublicKey ];
      };
    };
  };
  microvm = { inherit vcpu mem; };
}
