{ inputs, pkgs }:
let
  base =
    { lib, ... }:
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
        ../modules/shell/git.nix
      ];
      options.modules.users.primaryUser = lib.mkOption { type = lib.types.attrs; };
      config = {
        modules.users.primaryUser = {
          username = "alice";
          homeDirectory = "/home/alice";
          name = "Alice";
          email = "alice@example.invalid";
        };
        modules.shell.git.enable = true;
        users.users.alice = {
          isNormalUser = true;
          uid = 1000;
          linger = true;
        };
        home-manager.useGlobalPkgs = true;
        home-manager.users.alice.home.stateVersion = "26.05";
      };
    };
in
pkgs.testers.runNixOSTest {
  name = "git-credential-cache";
  nodes.machine = base;
  nodes.unmanaged = {
    imports = [ base ];
    modules.shell.git.credentialCache.enable = false;
  };
  testScript = ''
    import shlex
    import time

    machine.start()
    machine.wait_for_unit("user@1000.service")
    machine.wait_for_unit("home-manager-alice.service")

    def as_user(command, node=machine):
        return node.succeed(
            "${pkgs.util-linux}/bin/runuser -u alice -- "
            "env XDG_RUNTIME_DIR=/run/user/1000 "
            "${pkgs.bash}/bin/bash -c " + shlex.quote(command)
        )

    helper = as_user("${pkgs.git}/bin/git config --get-all credential.helper").splitlines()[0]
    request = "protocol=https\nhost=example.invalid\n\n"
    credential = "protocol=https\nhost=example.invalid\nusername=test\npassword=synthetic-test-only\n\n"

    def helper_command(action, data):
        return "printf %s " + shlex.quote(data) + " | " + shlex.quote(helper) + " " + action

    # A short-lived service is deliberately the first client, like T3's fetches.
    as_user("systemctl --user stop git-credential-cache.service")
    as_user(
        "systemd-run --user --wait --collect --pipe --unit=credential-producer "
        "${pkgs.bash}/bin/bash -c " + shlex.quote(helper_command("store", credential))
    )
    pid = as_user("systemctl --user show git-credential-cache.service -p MainPID --value").strip()
    assert int(pid) > 0
    group = as_user("systemctl --user show git-credential-cache.service -p ControlGroup --value")
    assert group.strip().endswith("/git-credential-cache.service"), group
    assert "password=synthetic-test-only" in as_user(helper_command("get", request))

    # An independent user service can consume the same cache after the producer exits.
    result = as_user(
        "systemd-run --user --wait --collect --pipe --unit=credential-consumer "
        "${pkgs.bash}/bin/bash -c " + shlex.quote(helper_command("get", request))
    )
    assert "password=synthetic-test-only" in result
    assert as_user("stat -c %a /home/alice/.cache/git/credential-systemd").strip() == "700"

    # Git exits after 30 seconds with no entries; the next daemon must still be systemd-owned.
    as_user(helper_command("erase", credential))
    time.sleep(35)
    next_pid = as_user("systemctl --user show git-credential-cache.service -p MainPID --value").strip()
    assert int(next_pid) > 0 and next_pid != pid, (pid, next_pid)
    as_user(helper_command("store", credential))
    assert "password=synthetic-test-only" in as_user(helper_command("get", request))
    as_user(helper_command("erase", credential))

    unmanaged.start()
    unmanaged.wait_for_unit("user@1000.service")
    unmanaged.wait_for_unit("home-manager-alice.service")
    helpers = as_user("${pkgs.git}/bin/git config --get-all credential.helper", unmanaged).splitlines()
    assert helpers == ["cache --timeout 86400", "${pkgs.git-credential-oauth}/bin/git-credential-oauth"], helpers
    assert as_user(
        "systemctl --user show git-credential-cache.service -p LoadState --value", unmanaged
    ).strip() == "not-found"
  '';
}
