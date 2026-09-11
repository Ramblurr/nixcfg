{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluate =
    settings: extra:
    (inputs.nixpkgs.lib.nixosSystem {
      modules = [
        ../modules/services/immich-machine-learning.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          users.users.root.openssh.authorizedKeys.keys = (import ./fixtures/immich/global.nix).pubKeys;
          modules.services.immich-machine-learning = settings;
        }
        extra
      ];
    }).config;
  disabled = evaluate { } { };
  cpu = evaluate { enable = true; } { };
  gpu = evaluate {
    enable = true;
    gpu = true;
  } { };
  colocated = evaluate { enable = true; } { services.postgresql.enable = true; };
  postgresOnly = evaluate { } { services.postgresql.enable = true; };
  conflict = evaluate { enable = true; } { services.immich.enable = true; };
  custom = evaluate {
    enable = true;
    host = "192.0.2.30";
    port = 3103;
    package = pkgs.immich-machine-learning;
    environment.MACHINE_LEARNING_REQUEST_THREADS = "2";
  } { };
  ml = gpu.systemd.services.immich-machine-learning;
in
assert lib.assertMsg (
  !(disabled.systemd.services ? immich-machine-learning)
) "Disabled ML must create no service";
assert lib.assertMsg (!(disabled.users.users ? immich)) "Disabled ML must create no identity";
assert lib.assertMsg (
  gpu.system.build.toplevel.drvPath != ""
) "GPU-enabled role must fully evaluate without GPU builds";
assert lib.assertMsg (
  !gpu.services.immich.enable && !(gpu.systemd.services ? immich-server)
) "ML must not enable the Immich server";
assert lib.assertMsg (
  !gpu.services.postgresql.enable && !(gpu.systemd.services ? postgresql-setup)
) "ML must not create PostgreSQL hooks";
assert lib.assertMsg (gpu.services.redis.servers == { }) "ML must not enable Redis";
assert lib.assertMsg (
  colocated.system.build.toplevel.drvPath != ""
) "Unrelated PostgreSQL must remain supported";
assert lib.assertMsg (
  (colocated.systemd.services.postgresql-setup.serviceConfig.ExecStartPost or [ ])
  == (postgresOnly.systemd.services.postgresql-setup.serviceConfig.ExecStartPost or [ ])
) "ML must not alter another PostgreSQL cluster's setup";
assert lib.assertMsg (
  !(builtins.tryEval conflict.system.build.toplevel.drvPath).success
) "Reject overlapping ownership by full Immich";
assert lib.assertMsg (
  ml.environment.IMMICH_HOST == "127.0.0.1" && ml.environment.IMMICH_PORT == "3003"
) "Default endpoint must be loopback-only";
assert lib.assertMsg (
  !builtins.elem 3003 gpu.networking.firewall.allowedTCPPorts
) "No automatic firewall exposure";
assert lib.assertMsg (
  ml.environment.XDG_RUNTIME_DIR == "/run/immich-machine-learning"
  && ml.serviceConfig.RuntimeDirectoryMode == "0700"
) "Control socket needs a private runtime directory";
assert lib.assertMsg (
  ml.serviceConfig.CacheDirectory == "immich" && ml.serviceConfig.ProtectSystem == "strict"
) "Only managed cache/runtime directories should be writable";
assert lib.assertMsg (
  !ml.serviceConfig.PrivateDevices
  && cpu.systemd.services.immich-machine-learning.serviceConfig.PrivateDevices
) "Device access must be opt-in";
assert lib.assertMsg (lib.all (g: builtins.elem g gpu.users.users.immich.extraGroups) [
  "video"
  "render"
]) "GPU identity requires device groups";
assert lib.assertMsg (
  !(cpu.systemd.services.immich-machine-learning.environment ? LD_LIBRARY_PATH)
) "CPU role must not inject NVIDIA libraries";
assert lib.assertMsg (
  custom.systemd.services.immich-machine-learning.environment.IMMICH_HOST == "192.0.2.30"
  && custom.systemd.services.immich-machine-learning.environment.IMMICH_PORT == "3103"
  &&
    custom.systemd.services.immich-machine-learning.environment.MACHINE_LEARNING_REQUEST_THREADS == "2"
  &&
    custom.systemd.services.immich-machine-learning.serviceConfig.ExecStart
    == lib.getExe pkgs.immich-machine-learning
) "Module must honor explicit service settings";
assert lib.assertMsg (
  gpu.environment.systemPackages == cpu.environment.systemPackages
  && gpu.system.extraDependencies == [ ]
) "Role must not install diagnostic environments";
pkgs.runCommand "immich-machine-learning-evaluation" { } "touch $out"
