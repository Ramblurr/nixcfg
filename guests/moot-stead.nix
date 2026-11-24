{
  vcpu,
  mem,
  zvolSize,
  builder ? null,
}:
{ config, lib, ... }:
let
  inherit (config.repo.secrets) global;
in
{
  system.stateVersion = "26.05";

  modules.microvm-guest = {
    host = "thingstead";
    hostFQDN = "thingstead.${config.site.net.mgmt.domainName}";
    mootWright = {
      enable = true;
      nix.remoteBuilder = lib.mkIf (builder != null) {
        hostName = "${builder}.${global.domain.tailnet}";
        publicHostKey = lib.fileContents ../config/ssh/${builder}-node-host.pub;
      };
    };
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
