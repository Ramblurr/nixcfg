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
    # StrictModes rejects an authorized-key symlink through group-writable /nix/store.
    environment.etc."ssh/microvm-host.pub" = {
      source = host.publicKeyFile;
      mode = "0444";
    };

    # Only the generator's VSOCK listener accepts this key; TCP SSH is unchanged.
    # Preserve existing administrative keys and restrict this listener to root.
    systemd.services."sshd-vsock@" = {
      overrideStrategy = "asDropin";
      serviceConfig.ExecStart = [
        ""
        "-${lib.getExe' config.services.openssh.package "sshd"} -i -D -f /etc/ssh/sshd_config -o 'AuthorizedKeysFile /etc/ssh/microvm-host.pub /etc/ssh/authorized_keys.d/%%u .ssh/authorized_keys' -o 'AllowUsers root'"
      ];
    };
  };
}
