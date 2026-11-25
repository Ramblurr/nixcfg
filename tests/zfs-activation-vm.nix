{ pkgs, ... }:
let
  # Exercise real systemd and NixOS activation, with only ZFS/SOPS I/O replaced.
  # Mount assertions use a real tmpfs mount, not a mocked mountpoint check.
  zfsFixture = pkgs.writeShellScriptBin "zfs" ''
    set -eu
    test ! -e /run/fail-zfs || exit 42
    case "$1" in
      list) test "$2" = pool/probe ;;
      get)
        case "$3" in
          mountpoint) printf 'pool/probe\tmountpoint\t/srv/probe\tlocal\n' ;;
          quota) printf 'pool/probe\tquota\t%s\tlocal\n' "$(cat /run/probe-quota 2>/dev/null || echo none)" ;;
          *) exit 1 ;;
        esac
        ;;
      set)
        test "$2" = quota=1G
        test "$3" = pool/probe
        echo 1G > /run/probe-quota
        ;;
      *) exit 1 ;;
    esac
  '';
  machine =
    { config, lib, ... }:
    {
      imports = [
        ../modules/zfs-attrs.nix
        ../hosts/mali/zfs-keys.nix
      ];
      options.sops.secrets = lib.mkOption {
        type = lib.types.attrsOf lib.types.attrs;
        default = { };
      };
      config = {
        system.stateVersion = "26.05";
        systemd.services.backdoor.unitConfig.DefaultDependencies = false;
        systemd.services.zfs-datasets.path = lib.mkForce [ zfsFixture ];
        virtualisation.memorySize = 1024;
        virtualisation.fileSystems."/srv/probe" = {
          device = "none";
          fsType = "tmpfs";
        };
        sops.secrets = {
          fastKey.path = "/run/test-keys/fast";
          tank2Key.path = "/run/test-keys/tank2";
        };
        systemd.services.sops-install-secrets-for-users = {
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            test ! -e /run/fail-secrets
            mkdir -p /run/test-keys
            printf fixture-key > /run/test-keys/fast
            printf fixture-key > /run/test-keys/tank2
          '';
        };
        systemd.services.zfs-import-fast = {
          requiredBy = [ "zfs-import.target" ];
          before = [ "zfs-import.target" ];
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = "touch /run/imported-fast";
        };
        systemd.services.zfs-import-tank2 = {
          requiredBy = [ "zfs-import.target" ];
          before = [ "zfs-import.target" ];
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = "touch /run/imported-tank2";
        };
        systemd.targets.zfs-import.unitConfig.DefaultDependencies = false;
        systemd.services.zfs-mount = {
          after = [ "zfs-import.target" ];
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = "true";
        };
        modules.zfs.datasets = {
          enable = true;
          properties."pool/probe".mountpoint = "/srv/probe";
          services."pool/probe" = [ "consumer" ];
        };
        systemd.services.consumer = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = "touch /srv/probe/consumer-started";
        };
        systemd.services.probe-at-final = {
          wantedBy = [ "final.target" ];
          before = [ "final.target" ];
          after = [
            "shutdown.target"
            "umount.target"
          ];
          unitConfig.DefaultDependencies = false;
          serviceConfig.Type = "oneshot";
          script = ''
            if ${pkgs.util-linux}/bin/mountpoint -q /srv/probe; then dataset=mounted; else dataset=unmounted; fi
            printf 'FINAL: sysinit=%s barrier=%s dataset=%s cycles=%s\n' \
              "$(systemctl show sysinit.target -p ActiveState --value)" \
              "$(systemctl show zfs-datasets.service -p ActiveState --value)" \
              "$dataset" "$(journalctl -b -q --grep='ordering cycle' | wc -l)" > /dev/ttyS0
          '';
        };
        systemd.services.key-import-probe = {
          inherit (config.systemd.services.zfs-import-fast) wants after preStart;
          serviceConfig.Type = "oneshot";
          script = "touch /run/probe-imported";
        };
        specialisation.updated.configuration = {
          modules.zfs.datasets.properties."pool/probe".quota = "1G";
          systemd.services.systemd-tmpfiles-resetup.preStart = ''
            test "$(cat /run/probe-quota)" = 1G
          '';
        };
        specialisation.mountchange.configuration = {
          systemd.services.zfs-mount.environment.ZFS_SERIAL_MOUNT = "1";
        };
        specialisation.importchange.configuration = {
          systemd.services.zfs-import-fast.environment.CHANGED = "1";
        };
      };
    };
in
pkgs.testers.runNixOSTest {
  name = "zfs-activation";
  nodes.machine = machine;
  nodes.mountfail =
    { lib, ... }:
    {
      imports = [ machine ];
      specialisation = lib.mkForce { };
      systemd.services.zfs-mount.script = lib.mkForce "exit 1";
    };

  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("consumer.service")
    machine.succeed("test -e /run/imported-fast && test -e /run/imported-tank2")
    protected = ["zfs-import-fast", "zfs-import-tank2", "zfs-mount", "zfs-datasets", "consumer"]
    identities = {
        unit: machine.succeed(f"systemctl show {unit} -p InvocationID --value").strip()
        for unit in protected
    }
    assert all(identities.values())

    with subtest("secret updates do not stop imports or system targets"):
        machine.succeed("systemctl stop sops-install-secrets-for-users.service")
        for unit in protected + ["sysinit.target", "multi-user.target"]:
            machine.succeed(f"systemctl is-active {unit}")
        machine.succeed("touch /run/fail-secrets")
        machine.fail("systemctl start key-import-probe.service")
        machine.succeed("systemctl is-failed sops-install-secrets-for-users.service")
        machine.succeed("test -s /run/test-keys/fast && test ! -e /run/probe-imported")
        machine.succeed("rm /run/fail-secrets; systemctl reset-failed sops-install-secrets-for-users.service")
        machine.succeed("systemctl start sops-install-secrets-for-users.service")
        machine.succeed("truncate -s 0 /run/test-keys/fast")
        machine.fail("systemctl start key-import-probe.service")
        machine.succeed("test ! -e /run/probe-imported; rm /run/test-keys/fast")
        machine.fail("systemctl start key-import-probe.service")
        machine.succeed("test ! -e /run/probe-imported")
        machine.succeed("systemctl restart sops-install-secrets-for-users.service")
        machine.succeed("systemctl start key-import-probe.service; test -e /run/probe-imported")
        for unit in protected:
            assert machine.succeed(f"systemctl show {unit} -p InvocationID --value").strip() == identities[unit]

    with subtest("a configuration switch reapplies properties without stopping the barrier"):
        machine.succeed("/run/current-system/specialisation/updated/bin/switch-to-configuration test", timeout=120)
        machine.succeed("test $(cat /run/probe-quota) = 1G")
        for unit in protected + ["sysinit.target", "multi-user.target"]:
            machine.succeed(f"systemctl is-active {unit}")
        for unit in protected:
            assert machine.succeed(f"systemctl show {unit} -p InvocationID --value").strip() == identities[unit]

    with subtest("failed reapplication blocks tmpfiles without stopping existing consumers"):
        machine.succeed("touch /run/fail-zfs")
        machine.fail("systemctl start systemd-tmpfiles-resetup.service")
        machine.succeed("systemctl is-failed zfs-datasets-reactivation.service")
        machine.succeed("systemctl is-active sysinit.target", "systemctl is-active consumer.service")
        machine.succeed("mountpoint /srv/probe")
        machine.succeed("rm /run/fail-zfs; systemctl reset-failed zfs-datasets-reactivation.service")
        machine.succeed("systemctl start systemd-tmpfiles-resetup.service")
        for unit in protected:
            assert machine.succeed(f"systemctl show {unit} -p InvocationID --value").strip() == identities[unit]

    with subtest("a failed zfs-mount at boot still blocks dataset readiness and sysinit.target"):
        mountfail.wait_until_succeeds("systemctl is-failed zfs-mount.service")
        mountfail.wait_until_succeeds('test -z "$(systemctl list-jobs --no-legend)"', timeout=120)
        for unit in ["zfs-datasets.service", "sysinit.target", "consumer.service"]:
            mountfail.fail(f"systemctl is-active {unit}")
        mountfail.fail("test -e /srv/probe/consumer-started")

    with subtest("a switch that restarts zfs-mount or a pool import keeps sysinit.target and the switch alive"):
        session = "systemd-run --scope -p Requires=sysinit.target -p After=sysinit.target "

        def invocation(unit):
            return machine.succeed(f"systemctl show {unit} -p InvocationID --value").strip()

        def switch(toplevel):
            machine.succeed(session + f"{toplevel}/bin/switch-to-configuration test", timeout=180)
            machine.wait_until_succeeds('test -z "$(systemctl list-jobs --no-legend)"', timeout=120)

        switch("/run/booted-system")
        barrier = {unit: invocation(unit) for unit in ["sysinit.target", "zfs-datasets.service"]}
        for spec, changed in [("mountchange", "zfs-mount.service"), ("importchange", "zfs-import-fast.service")]:
            for toplevel in [f"/run/booted-system/specialisation/{spec}", "/run/booted-system"]:
                before = invocation(changed)
                switch(toplevel)
                assert invocation(changed) != before, f"{toplevel} did not restart {changed}"
                for unit, identity in barrier.items():
                    assert invocation(unit) == identity, f"{toplevel} restarted {unit}"
                for unit in [changed, "zfs-mount.service", "consumer.service", "multi-user.target"]:
                    machine.succeed(f"systemctl is-active {unit}")

    with subtest("shutdown stops dataset readiness with the dataset mounts"):
        machine.fail("journalctl -b --grep='ordering cycle'")
        machine.execute("systemctl poweroff --no-block")
        machine.wait_for_console_text("FINAL: sysinit=inactive barrier=inactive dataset=unmounted cycles=0", timeout=60)
        machine.wait_for_shutdown()
  '';
}
