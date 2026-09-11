{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluate =
    enable: localPostgres:
    (inputs.nixpkgs.lib.nixosSystem {
      modules = [
        ../modules/services/immich-worker.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          fileSystems."/var/lib/immich" = {
            device = "192.0.2.10:/exports/immich";
            fsType = "nfs";
          };
          users.users.root.openssh.authorizedKeys.keys = (import ./fixtures/immich/global.nix).pubKeys;
          modules.services.immich-worker.enable = enable;
          services.postgresql.enable = localPostgres;
          services.immich = {
            database.host = "192.0.2.20";
            redis = {
              host = "192.0.2.20";
              port = 6379;
            };
            secretsFile = "/run/secrets/immich.env";
            machine-learning.environment.IMMICH_HOST = lib.mkForce "192.0.2.30";
          };
        }
      ];
    }).config;
  cfg = evaluate true false;
  disabled = evaluate false false;
  colocated = evaluate true true;
  server = cfg.systemd.services.immich-server;
in
assert lib.assertMsg (!disabled.services.immich.enable) "Disabled worker must not enable Immich";
assert lib.assertMsg (cfg.system.build.toplevel.drvPath != "") "Enabled worker must fully evaluate";
assert lib.assertMsg (
  !(builtins.tryEval colocated.system.build.toplevel.drvPath).success
) "Worker must reject a local PostgreSQL cluster rather than suppressing its setup hooks";
assert lib.assertMsg (
  cfg.services.immich.settings == null && !(server.environment ? IMMICH_CONFIG_FILE)
) "Settings must remain UI-managed";
assert lib.assertMsg (
  server.environment.IMMICH_WORKERS_INCLUDE == "microservices"
) "Worker must use upstream background role";
assert lib.assertMsg (
  !cfg.services.postgresql.enable && !cfg.systemd.services.postgresql-setup.enable
) "Worker must not start or modify local PostgreSQL";
assert lib.assertMsg (
  !cfg.services.immich.redis.enable
  && !(server.environment ? DB_URL)
  && !(server.environment ? REDIS_SOCKET)
) "Worker must connect to remote state services";
assert lib.assertMsg (
  server.serviceConfig.EnvironmentFile == "/run/secrets/immich.env"
) "Credentials must be runtime files";
assert lib.assertMsg (
  server.serviceConfig.StateDirectory == "" && builtins.elem "var-lib-immich.mount" server.bindsTo
) "NFS must be required without client-root ownership changes";
assert lib.assertMsg (
  !cfg.services.immich.machine-learning.enable && !(cfg.systemd.services ? immich-machine-learning)
) "Worker must use only the authenticated remote ML endpoint";
assert lib.assertMsg (lib.all (group: builtins.elem group cfg.users.users.immich.extraGroups) [
  "video"
  "render"
]) "Worker identity must have standard GPU device groups";
assert lib.assertMsg (
  !builtins.elem 2283 cfg.networking.firewall.allowedTCPPorts
  && !builtins.elem 3003 cfg.networking.firewall.allowedTCPPorts
) "Worker ports must not be broadly exposed";
pkgs.runCommand "immich-worker-evaluation" { } "touch $out"
