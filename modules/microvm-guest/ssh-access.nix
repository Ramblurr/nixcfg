{ config, lib, ... }:
let
  cfg = config.modules.microvm-guest;
  registry = import ../../config/microvm-ssh.nix;
  host = registry.${cfg.host} or null;
  guest = if host == null then null else host.guests.${config.networking.hostName} or null;
in
{
  config = lib.mkIf (cfg.enable && guest != null) {
    microvm.vsock = {
      cid = guest.cid;
      ssh.enable = true;
    };
    # Standard root authorization applies to both VSOCK and TCP SSH.
    users.users.root.openssh.authorizedKeys.keyFiles = [ host.publicKeyFile ];
  };
}
