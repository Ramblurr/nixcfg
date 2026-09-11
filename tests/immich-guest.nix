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
  cfg.services.immich.environment.IMMICH_WORKERS_INCLUDE == "api"
  && !cfg.services.immich.machine-learning.enable
  && cfg.services.immich.accelerationDevices == [ ]
) "Guest must remain API-only without GPU or ML fallback";
assert lib.assertMsg (
  cfg.services.immich.environment.IMMICH_MACHINE_LEARNING_URL == "http://127.0.0.1:3004"
  && cfg.modules.services.immich-ml-proxy.upstreamAddress == "10.9.4.24"
  && cfg.modules.services.immich-ml-proxy.allowedUser == "immich"
) "Guest ML traffic must use the UID-restricted local mTLS proxy";
assert lib.assertMsg (
  builtins.elem "immich-ml-client-proxy.service" cfg.systemd.services.immich-server.requires
  && builtins.elem "immich-ml-client-proxy" cfg.modules.microvm-guest.hostSecrets.services
) "Guest credentials and proxy must start before Immich";
assert lib.assertMsg (
  lib.hasInfix "meta skuid 3024" cfg.networking.nftables.tables.immich-ml-client-access.content
  && lib.hasInfix "ip6 daddr ::1" cfg.networking.nftables.tables.immich-ml-client-access.content
) "Local ML proxy access must restrict IPv4 and IPv6 loopback by UID";
assert lib.assertMsg (
  cfg.services.immich.settings == null
  && !(cfg.systemd.services.immich-server.environment ? IMMICH_CONFIG_FILE)
) "Guest settings must remain UI-managed";
assert lib.assertMsg (
  cfg.services.postgresql.settings.listen_addresses == "172.20.20.23,10.9.4.23"
  && cfg.services.redis.servers.immich.bind == "172.20.20.23 10.9.4.23"
  && cfg.services.redis.servers.immich.settings.appendonly == "yes"
) "Private state listeners and durable queue must be explicit";
assert lib.assertMsg (
  builtins.elem "var-lib-immich.mount" cfg.systemd.services.immich-server.bindsTo
  && cfg.systemd.services.immich-server.serviceConfig.StateDirectory == ""
) "API must stop with NFS and leave export ownership to Mali";
assert lib.assertMsg (
  cfg.services.immich.secretsFile == "/var/lib/immich-secrets/environment"
  && cfg.services.redis.servers.immich.requirePassFile == "/var/lib/immich-secrets/redis-password"
) "Guest credentials must be runtime file paths";
assert lib.assertMsg (
  links.imhome-svc.macvtap.link == "vlan-svc"
  && links.imhome-data.macvtap.link == "vlan-data"
  && links.imhome-prim.macvtap.link == "vlan-prim"
) "Each guest network must use its own VLAN parent";
assert lib.assertMsg (
  links.imhome-svc.type == "macvtap"
  && links.imhome-data.macvtap.mode == "bridge"
  && links.imhome-prim.macvtap.mode == "bridge"
) "Guest must share the host macvlan segment";
assert lib.assertMsg (lib.all
  (
    net:
    builtins.hasAttr "10-${net}" cfg.systemd.network.links
    && cfg.systemd.network.links."10-${net}".linkConfig.Name == net
    && cfg.systemd.network.links."10-${net}".matchConfig.MACAddress == links."imhome-${net}".mac
  )
  [
    "svc"
    "data"
    "prim"
  ]
) "Guest interface naming must precede 99-default.link so firewall interface rules match";
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
