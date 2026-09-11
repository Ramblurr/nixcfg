{ pkgs, ... }:
pkgs.testers.runNixOSTest {
  name = "databasus-permissions";
  nodes.machine =
    { lib, ... }:
    {
      imports = [
        ../modules/services/databasus
        ../modules/services/podman.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/zfs-attrs.nix
      ];
      options = {
        virtualisation.quadlet.enable = lib.mkEnableOption "Quadlet";
        modules.boot.zfs.rootPool = lib.mkOption {
          type = lib.types.str;
          default = "rpool";
        };
        modules.services.caddy.routes = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
      };
      config = {
        system.stateVersion = "26.05";
        modules.services.databasus = {
          enable = true;
          domain = "databasus.example.test";
        };
        # Exercise the production tmpfiles rule with real PostgreSQL and the
        # pinned image's numeric identities, without pulling an OCI image or ZFS.
        modules.zfs.datasets.enable = lib.mkForce false;
        virtualisation.oci-containers.containers.databasus.autoStart = lib.mkForce false;
        virtualisation.memorySize = 1024;
        users.users.postgres.uid = lib.mkForce 999;
        users.groups.postgres.gid = lib.mkForce 999;
        services.postgresql = {
          enable = true;
          package = pkgs.postgresql_17;
          dataDir = "/var/lib/databasus/pgdata";
          settings.port = 5437;
          ensureDatabases = [ "databasus" ];
        };
        # The OCI entrypoint normally creates this before starting PostgreSQL.
        systemd.tmpfiles.rules = [ "d /var/lib/databasus/pgdata 0700 999 999 -" ];
        environment.systemPackages = [ pkgs.util-linux ];
        specialisation.updated.configuration.environment.etc."databasus-test-generation".text = "updated";
      };
    };

  testScript = ''
    start_all()
    machine.wait_for_unit("postgresql.service")
    sql = "sudo -u postgres psql -p 5437 -d databasus -v ON_ERROR_STOP=1 -c "
    machine.succeed(sql + '"CREATE TABLE probe (value integer); INSERT INTO probe VALUES (1); CHECKPOINT;"')
    invocation = machine.succeed("systemctl show postgresql -p InvocationID --value").strip()

    def check_access():
        assert machine.succeed("stat -c '%u:%g:%a' /var/lib/databasus").strip() == "65532:999:750"
        machine.succeed("setpriv --reuid=65532 --regid=999 --clear-groups test -w /var/lib/databasus")
        machine.succeed("sudo -u postgres test -r /var/lib/databasus/pgdata/global/pg_control")
        machine.fail("setpriv --reuid=65534 --regid=65534 --clear-groups test -x /var/lib/databasus")
        machine.succeed(sql + '"INSERT INTO probe VALUES (2); CHECKPOINT; SELECT * FROM probe;"')
        assert machine.succeed("systemctl show postgresql -p InvocationID --value").strip() == invocation

    with subtest("fresh installation grants both container identities access, not other users"):
        check_access()

    with subtest("activation repairs legacy ownership without restarting PostgreSQL"):
        machine.succeed("chown 0:0 /var/lib/databasus")
        machine.fail("sudo -u postgres test -r /var/lib/databasus/pgdata/global/pg_control")
        machine.succeed("systemctl start systemd-tmpfiles-resetup.service")
        check_access()

    with subtest("a NixOS switch preserves database access and process identity"):
        machine.succeed("/run/current-system/specialisation/updated/bin/switch-to-configuration test", timeout=120)
        check_access()

    with subtest("PostgreSQL readiness rejects a stopped database"):
        machine.succeed("pg_isready -h 127.0.0.1 -p 5437 -U postgres -d databasus")
        machine.succeed("systemctl stop postgresql")
        machine.fail("pg_isready -h 127.0.0.1 -p 5437 -U postgres -d databasus")
  '';
}
