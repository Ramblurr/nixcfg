{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  guest = inputs.self.lib.nixcfg.mkGuest "immich-home" {
    extraModules = [
      {
        repo.secretFiles = {
          global = lib.mkForce ./fixtures/immich/global.nix;
          site = lib.mkForce ./fixtures/immich/site.nix;
          local = lib.mkForce ./fixtures/immich/local.nix;
        };
      }
    ];
  };
  cfg = guest.config;
  links = builtins.listToAttrs (
    map (iface: {
      name = iface.id;
      value = iface;
    }) cfg.microvm.interfaces
  );
in
assert lib.assertMsg (cfg.system.build.toplevel.drvPath != "") "Guest must fully evaluate";
assert lib.assertMsg (cfg.microvm.devices == [ ]) "Guest must not receive GPU devices";
assert lib.assertMsg (
  links.imhome-svc.macvtap.link == "vlan-svc" && links.imhome-data.macvtap.link == "vlan-data"
) "Each guest network must use its own VLAN parent";
assert lib.assertMsg (
  links.imhome-svc.type == "macvtap" && links.imhome-data.macvtap.mode == "bridge"
) "Guest must share the host macvlan segment";
assert lib.assertMsg (
  cfg.microvm.mem == 4096 && cfg.microvm.vcpu == 2
) "Guest resource budget changed";
assert lib.assertMsg (
  cfg.fileSystems."/var/lib".neededForBoot && cfg.fileSystems."/var/lib/immich".fsType == "nfs"
) "State must persist locally and media must use NFS";
assert lib.assertMsg (
  cfg.users.users.immich.uid == 3024 && cfg.users.groups.immich.gid == 3024
) "NFS service identity must be stable";
assert lib.assertMsg (
  !cfg.services.openssh.settings.PasswordAuthentication
  && !cfg.services.openssh.settings.PermitEmptyPasswords
) "Guest SSH must be key-only";
assert lib.assertMsg (
  builtins.length cfg.services.openssh.hostKeys == 1
  && (builtins.head cfg.services.openssh.hostKeys).path == "/var/lib/sshd/ssh_host_ed25519_key"
) "SSH host identity must persist outside the ephemeral root";
pkgs.runCommand "immich-guest-evaluation" { } "touch $out"
