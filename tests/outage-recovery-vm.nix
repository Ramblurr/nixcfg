{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  fakeOp = pkgs.writeShellScriptBin "op" ''
    test -e /run/connect-up || exit 1
    printf %s credential-value
  '';
  sleeper = "${pkgs.coreutils}/bin/sleep infinity";
  checkCredential = ''test "$(cat "$CREDENTIALS_DIRECTORY/password")" = credential-value'';
  envSetup = consumer: {
    wantedBy = [ "${consumer}.service" ];
    before = [ "${consumer}.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      RuntimeDirectory = consumer;
    };
    script = ''
      ${checkCredential}
      echo SET=1 > /run/${consumer}/env
    '';
  };
  envConsumer = name: restartSec: {
    wantedBy = [ "multi-user.target" ];
    partOf = [ "${name}-env-setup.service" ];
    after = [ "${name}-env-setup.service" ];
    serviceConfig = {
      EnvironmentFile = "/run/${name}/env";
      ExecStart = sleeper;
      Restart = "on-failure";
      RestartSec = restartSec;
    };
  };
  changed.systemd.services.app-env-setup.environment.CHANGED = "1";
in
pkgs.testers.runNixOSTest {
  name = "outage-recovery";

  nodes.machine = {
    imports = [
      inputs.sops-nix.nixosModules.sops
      ../modules/services/onepassword-systemd-credentials.nix
    ];

    options.site = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    config = {
      networking.hostName = "debord";
      site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
      system.stateVersion = "26.05";
      system.switch.enable = true;
      specialisation.changed.configuration = changed;
      systemd.tmpfiles.rules = [ "f /run/provider-token 0600 root root - runtime-bootstrap-token" ];

      modules.services.onepassword-systemd-credentials = {
        package = fakeOp;
        bootstrapTokenFile = "/run/provider-token";
        consumers = {
          app-env-setup.password = "op://test/App/password";
          status-env-setup.password = "op://test/Status/password";
          kept-env-setup.password = "op://test/Kept/password";
          role.password = "op://test/Role/password";
        };
        microvmSecrets = {
          ward.API_TOKEN = "op://test/Ward/token";
          parked.API_TOKEN = "op://test/Parked/token";
        };
      };

      systemd.targets.microvms = {
        wantedBy = [ "multi-user.target" ];
        wants = [
          "microvm@ward.service"
          "microvm@parked.service"
        ];
      };

      systemd.services = {
        app-env-setup = envSetup "app";
        app = envConsumer "app" "5s";
        status-env-setup = envSetup "status";
        status = envConsumer "status" "30s";
        kept-env-setup = envSetup "kept";
        kept = envConsumer "kept" "5s";

        role = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            Restart = "on-failure";
            RestartSec = "30s";
          };
          script = checkCredential;
        };

        "microvm@" = {
          wants = [ "microvm-virtiofsd@%i.service" ];
          after = [ "microvm-virtiofsd@%i.service" ];
          serviceConfig = {
            ExecStart = sleeper;
            Restart = "always";
            RestartSec = "5s";
          };
        };
        "microvm-virtiofsd@" = {
          partOf = [ "microvm@%i.service" ];
          serviceConfig = {
            ExecStart = sleeper;
            Restart = "always";
            RestartSec = "5s";
          };
        };
        "microvm@ward".overrideStrategy = "asDropin";
        "microvm-virtiofsd@ward".overrideStrategy = "asDropin";
        "microvm@parked".overrideStrategy = "asDropin";
        "microvm-virtiofsd@parked".overrideStrategy = "asDropin";
      };
    };
  };

  testScript = ''
    import time

    def state(unit):
        return machine.succeed(f"systemctl show -p ActiveState --value {unit}").strip()

    def starts(unit):
        return int(machine.succeed(f"journalctl -b -u {unit} | grep -c 'Starting ' || true").strip())

    def timed(command):
        started = time.monotonic()
        status, output = machine.execute(command)
        return status, time.monotonic() - started, output

    machine.wait_for_unit("multi-user.target")

    with subtest("with Connect down nothing that needs a credential runs"):
        for unit in ["app-env-setup", "status-env-setup", "role", "microvm-secrets-ward"]:
            machine.wait_until_succeeds(f"journalctl -b -u {unit} | grep -q 'Failed with result'", timeout=120)
        for unit in ["app", "status", "kept", "microvm@ward", "microvm-virtiofsd@ward"]:
            assert state(unit) != "active", f"{unit} runs without its prerequisite"

    with subtest("each retry of a consumer or a setup runs the setup again"):
        for unit in ["app-env-setup", "status-env-setup", "role", "microvm-secrets-ward"]:
            machine.wait_until_succeeds(f"test $(journalctl -b -u {unit} | grep -c 'Starting ') -ge 2", timeout=150)
        for unit in ["app", "status", "microvm@ward"]:
            machine.succeed(f"journalctl -b -u {unit} | grep -q 'Scheduled restart'")

    with subtest("a manual start during the outage returns"):
        for unit in ["app.service", "microvm@ward.service"]:
            status, elapsed, _ = timed(f"timeout 300 systemctl start {unit}")
            print(f"systemctl start {unit}: status {status} after {elapsed:.1f}s")
            assert status not in (0, 124) and elapsed < 60, f"{unit}: status {status} after {elapsed:.1f}s"

    with subtest("a switch during the outage returns"):
        status, elapsed, output = timed(
            "timeout 300 /run/booted-system/specialisation/changed/bin/switch-to-configuration test 2>&1"
        )
        print(output[-2000:])
        print(f"switch: status {status} after {elapsed:.1f}s")
        assert status != 124 and elapsed < 90, f"switch status {status} after {elapsed:.1f}s"
        machine.succeed("systemctl show -p Environment --value app-env-setup.service | grep -q CHANGED=1")

    with subtest("stopping a consumer or a guest by hand ends its retries"):
        machine.succeed("systemctl stop kept.service microvm@parked.service")
        machine.succeed("systemctl reset-failed kept-env-setup.service microvm-secrets-parked.service")
        stopped = {unit: starts(unit) for unit in ["kept-env-setup", "microvm-secrets-parked"]}

    with subtest("everything comes up once Connect answers"):
        machine.succeed("touch /run/connect-up")
        for unit in ["app", "status", "role", "microvm-virtiofsd@ward", "microvm@ward"]:
            machine.wait_until_succeeds(f"systemctl is-active {unit}.service", timeout=150)

    with subtest("units stopped by hand stay stopped"):
        time.sleep(40)
        for unit in ["kept", "kept-env-setup", "microvm@parked", "microvm-secrets-parked"]:
            assert state(unit) == "inactive", f"{unit} came back after an explicit stop: {state(unit)}"
        for unit, count in stopped.items():
            assert starts(unit) == count, f"{unit} ran again after its consumer was stopped"
        machine.succeed("test -z \"$(systemctl list-jobs --no-legend)\"")

    with subtest("restarting a setup still restarts its consumer"):
        before = machine.succeed("systemctl show -p InvocationID --value app.service").strip()
        machine.succeed("systemctl restart app-env-setup.service")
        machine.wait_for_unit("app.service")
        after = machine.succeed("systemctl show -p InvocationID --value app.service").strip()
        assert before != after, "app kept running on the old environment"
  '';
}
