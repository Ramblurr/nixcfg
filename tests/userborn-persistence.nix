{ pkgs, ... }:
let
  inherit (pkgs) lib;
  nativeSubids = lib.versionAtLeast pkgs.userborn.version "1.0.0";
  node =
    { config, ... }:
    {
      imports = [
        ../modules/users/userborn.nix
        ../modules/impermanence
      ];
      options.modules.users.enable = lib.mkEnableOption "shared user configuration";
      config = {
        modules.users.enable = true;
        services.userborn.enable = true;
        users.mutableUsers = false;
        system.stateVersion = "26.05";
        virtualisation = {
          memorySize = 1536;
          diskSize = 3072;
          useBootLoader = true;
          # Do not let the VM module replace the production account filesystem.
          fileSystems = lib.mkForce { };
        };
        fileSystems."/" = {
          device = config.virtualisation.rootDevice;
          fsType = "ext4";
        };
        boot.initrd.systemd.enable = lib.mkIf (lib.versionOlder config.system.nixos.release "26.11") true;
        # Simulate rollback of all volatile account state, keeping /persist and
        # the store/bootloader so the selected generation survives a real reboot.
        boot.initrd.systemd.services.reset-volatile-account-state = {
          wantedBy = [ "initrd.target" ];
          requires = [ "sysroot.mount" ];
          after = [ "sysroot.mount" ];
          before = [
            "sysroot-var-lib-userborn.mount"
            "initrd-switch-root.target"
          ];
          unitConfig.DefaultDependencies = false;
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkgs.coreutils}/bin/rm -rf /sysroot/etc /sysroot/var";
          };
        };
        users.groups = {
          zservice = { };
          retired = lib.mkIf (!config.modules.impermanence.enable) { };
          anew = lib.mkIf config.modules.impermanence.enable { };
        };
        users.users = {
          zservice = {
            isSystemUser = true;
            group = "zservice";
          };
          retired = lib.mkIf (!config.modules.impermanence.enable) {
            isSystemUser = true;
            group = "retired";
          };
          anew = lib.mkIf config.modules.impermanence.enable {
            isSystemUser = true;
            group = "anew";
          };
          alice = {
            isNormalUser = true;
            uid = 1000;
          };
          invoiceninja2 = {
            isNormalUser = true;
            uid = 3015;
          };
        };
        specialisation.persistent.configuration.modules.impermanence.enable = true;
      };
    };
in
pkgs.testers.runNixOSTest {
  name = "userborn-persistence-${pkgs.userborn.version}";
  nodes = {
    machine = node;
    unseeded = {
      imports = [ node ];
      modules.impermanence.enable = true;
    };
  };
  testScript = ''
    import shlex

    native_subids = ${if nativeSubids then "True" else "False"}
    files = "passwd group shadow subuid subgid"
    machine.start()
    machine.wait_for_unit("multi-user.target")
    baseline = machine.succeed("readlink -f /run/current-system").strip()
    updated = baseline + "/specialisation/persistent"
    original_uid = machine.succeed("id -u zservice").strip()
    original_gid = machine.succeed("id -g zservice").strip()
    retired_uid = machine.succeed("id -u retired").strip()
    ranges = machine.succeed("sort /etc/subuid")
    assert "alice:65636000:65536" in ranges
    assert "invoiceninja2:362144:65536" in ranges
    assert machine.succeed("sort /etc/subgid") == ranges
    machine.succeed("mkdir -p /persist; touch /persist/owned; chown zservice:zservice /persist/owned; chmod 0600 /persist/owned")

    def check_identity():
        assert machine.succeed("id -u zservice").strip() == original_uid
        assert machine.succeed("id -g zservice").strip() == original_gid
        assert machine.succeed("stat -c '%u:%g' /persist/owned").strip() == f"{original_uid}:{original_gid}"
        machine.succeed("sudo -u zservice test -w /persist/owned")
        assert machine.succeed("sort /etc/subuid") == ranges
        assert machine.succeed("sort /etc/subgid") == ranges
        machine.succeed("sudo -u alice getent passwd zservice; sudo -u alice getent group zservice")
        machine.fail("sudo -u alice test -r /etc/shadow")
        machine.succeed("runuser -u alice -- unshare --user --map-auto -- true")

    def switch(path):
        machine.succeed(f"nix-env --profile /nix/var/nix/profiles/system --set {shlex.quote(path)}")
        machine.succeed(f"{shlex.quote(path)}/bin/switch-to-configuration switch", timeout=180)

    with subtest("seed the current database and migrate without renumbering"):
        machine.succeed("install -d -m 0755 /persist/var/lib/userborn")
        machine.succeed("cp -a /var/lib/userborn/. /persist/var/lib/userborn/")
        machine.succeed(f"for f in {files}; do cp -L --preserve=all /etc/$f /persist/var/lib/userborn/$f; cmp -s /etc/$f /persist/var/lib/userborn/$f; done")
        machine.succeed("mount --bind /persist/var/lib/userborn /var/lib/userborn")
        machine.succeed(f"for f in {files}; do if mountpoint -q /etc/$f; then umount /etc/$f; fi; done")
        switch(updated)
        check_identity()
        assert machine.succeed("id -u retired").strip() == retired_uid
        assert machine.succeed("passwd -S retired").split()[1] == "L"
        new_uid = machine.succeed("id -u anew").strip()
        assert new_uid not in (original_uid, retired_uid)
        machine.succeed("test /var/lib/userborn/passwd -ef /persist/var/lib/userborn/passwd")
        machine.succeed("test /etc/passwd -ef /persist/var/lib/userborn/passwd")

    with subtest("cold boot resets volatile state without losing accounts or mappings"):
        machine.succeed("touch /etc/volatile-account-test /var/volatile-account-test; sync")
        machine.shutdown()
        machine.start()
        machine.wait_for_unit("multi-user.target")
        machine.fail("test -e /etc/volatile-account-test")
        machine.fail("test -e /var/volatile-account-test")
        machine.succeed("findmnt --mountpoint /var/lib/userborn")
        check_identity()
        assert machine.succeed("id -u retired").strip() == retired_uid
        machine.succeed("test /etc/passwd -ef /persist/var/lib/userborn/passwd")
        machine.succeed("test ! -L /etc/subuid; test ! -L /etc/subgid")
        if native_subids:
            machine.succeed("test /etc/subuid -ef /var/lib/userborn/subuid; test /etc/subgid -ef /var/lib/userborn/subgid")

    with subtest("ordinary reactivation preserves identity and effective subordinate files"):
        switch(updated)
        check_identity()
        machine.succeed("test ! -L /etc/subuid; test ! -L /etc/subgid")
        if native_subids:
            machine.succeed("test /etc/subuid -ef /var/lib/userborn/subuid; test /etc/subgid -ef /var/lib/userborn/subgid")

    with subtest("a partially seeded database is rejected without recreating missing files"):
        machine.succeed("systemctl stop userborn.service && umount /var/lib/userborn/shadow && mv /var/lib/userborn/shadow /var/lib/userborn/shadow.saved")
        machine.fail("systemctl start userborn.service")
        machine.fail("test -e /var/lib/userborn/shadow")
        machine.succeed("mv /var/lib/userborn/shadow.saved /var/lib/userborn/shadow && systemctl reset-failed userborn.service && systemctl start userborn.service")
        if native_subids:
            machine.succeed("systemctl start etc-subuid.mount etc-subgid.mount")
        check_identity()

    with subtest("rollback restores ordinary account files before the old generation runs"):
        machine.succeed("install -d -m 0700 /run/userborn-rollback")
        machine.succeed(f"for f in {files}; do cp -L --preserve=all /etc/$f /run/userborn-rollback/$f; done")
        if native_subids:
            machine.succeed("umount /etc/subuid /etc/subgid")
        machine.succeed(f"for f in {files}; do cp --remove-destination --preserve=all /run/userborn-rollback/$f /etc/$f; done")
        # setup-etc otherwise unlinks even manually restored ordinary files.
        machine.succeed("awk '$0 != \"passwd\" && $0 != \"group\" && $0 != \"shadow\" && $0 != \"subuid\" && $0 != \"subgid\"' /etc/.clean > /run/userborn-rollback/clean; cp /run/userborn-rollback/clean /etc/.clean")
        switch(baseline)
        check_identity()
        machine.succeed("test ! -L /etc/passwd; test ! -L /etc/group; test ! -L /etc/shadow")
        assert machine.succeed("id -u retired").strip() == retired_uid
        assert machine.succeed("id -u anew").strip() == new_uid
        assert machine.succeed("passwd -S anew").split()[1] == "L"

    with subtest("an unseeded database is rejected without allocating new identities"):
        unseeded.start()
        unseeded.wait_until_succeeds("systemctl is-failed userborn.service", timeout=60)
        unseeded.succeed("systemctl is-failed userborn.service && test ! -e /var/lib/userborn/passwd && test ! -e /var/lib/userborn/group && test ! -e /var/lib/userborn/shadow")
        unseeded.crash()
  '';
}
