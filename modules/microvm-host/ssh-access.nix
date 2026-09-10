{ config, inputs, lib, pkgs, ... }:
let
  registry = import ../../config/microvm-ssh.nix;
  host = registry.${config.networking.hostName} or null;
  identity = "/var/lib/microvm-ssh/id_ed25519";
  upstream = pkgs.callPackage "${inputs.microvm}/pkgs/microvm-command.nix" {
    inherit (config.microvm) stateDir;
  };
  # Upstream -s overrides ssh_config's host verification policy. Keep its routing
  # and command handling, but let our strict per-guest configuration take effect.
  verifiedMicrovm = pkgs.runCommand "microvm-verified-ssh" { } ''
    mkdir -p "$out/bin"
    cp ${upstream}/bin/microvm "$out/bin/microvm"
    chmod u+w "$out/bin/microvm"
    substituteInPlace "$out/bin/microvm" --replace-fail \
      'ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null' 'ssh'
  '';
in
{
  config = lib.mkIf (config.modules.microvm-host.enable && host != null) {
    assertions = [{
      assertion = let cids = map (guest: guest.cid) (builtins.attrValues host.guests);
        in builtins.length cids == builtins.length (lib.unique cids);
      message = "Host-owned MicroVM SSH requires unique VSOCK CIDs on each host.";
    }];
    environment.persistence."/persist".directories = [{
      directory = "/var/lib/microvm-ssh";
      user = "root";
      group = "root";
      mode = "0700";
    }];
    environment.systemPackages = [ (lib.hiPrio verifiedMicrovm) ];
    programs.ssh = {
      extraConfig = lib.mkBefore (lib.concatStringsSep "\n" (lib.mapAttrsToList (name: guest: ''
        Host vsock/${toString guest.cid} vsock-mux/${config.microvm.stateDir}/${name}/notify.vsock
          User root
          IdentityFile ${identity}
          IdentityAgent none
          IdentitiesOnly yes
          BatchMode yes
          HostKeyAlias microvm-${name}
          StrictHostKeyChecking yes
          UserKnownHostsFile /dev/null
      '') host.guests));
      knownHosts = lib.mapAttrs' (name: guest: lib.nameValuePair "microvm-${name}" {
        hostNames = [ "microvm-${name}" ];
        publicKeyFile = guest.hostKeyFile;
      }) host.guests;
    };
  };
}
