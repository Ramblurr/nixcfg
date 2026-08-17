{ inputs, pkgs }:
let
  fakeOp = pkgs.writeShellScriptBin "op" ''
    set -eu

    test "''${OP_CONNECT_HOST:-}" = "http://192.0.2.22:8080"
    test "''${OP_CONNECT_TOKEN:-}" = "runtime-bootstrap-token"
    test "$1" = "read"
    test "$2" = "--no-newline"

    case "$3" in
      op://test/Example/password)
        printf %s dynamic-credential-value
        ;;
      op://test/Example/empty)
        ;;
      op://test/Example/oversized)
        ${pkgs.coreutils}/bin/head -c 1048577 /dev/zero | ${pkgs.coreutils}/bin/tr '\0' x
        ;;
      op://test/Example/failure)
        echo "synthetic Connect failure" >&2
        exit 1
        ;;
      *)
        echo "unexpected reference" >&2
        exit 1
        ;;
    esac
  '';
in
pkgs.testers.runNixOSTest {
  name = "onepassword-systemd-credentials";

  nodes.machine = {
    imports = [
      inputs.sops-nix.nixosModules.sops
      ../modules/services/onepassword-systemd-credentials.nix
    ];

    options.site = pkgs.lib.mkOption {
      type = pkgs.lib.types.attrs;
      default = { };
    };

    config = {
      networking.hostName = "debord";
      site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
      system.stateVersion = "26.05";

      modules.services.onepassword-systemd-credentials = {
        package = fakeOp;
        bootstrapTokenFile = "/run/onepassword-provider-test-token";
        consumers = {
          credential-success.password = "op://test/Example/password";
          credential-empty.password = "op://test/Example/empty";
          credential-oversized.password = "op://test/Example/oversized";
          credential-failure.password = "op://test/Example/failure";
        };
      };

      systemd.services.prepare-provider-token = {
        serviceConfig.Type = "oneshot";
        script = ''
          umask 077
          printf %s runtime-bootstrap-token > /run/onepassword-provider-test-token
        '';
      };

      systemd.services.credential-success = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = dynamic-credential-value
          touch /run/credential-success
        '';
      };

      systemd.services.credential-empty = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-empty-unexpected";
      };

      systemd.services.credential-oversized = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-oversized-unexpected";
      };

      systemd.services.credential-failure = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-failure-unexpected";
      };

      systemd.services.credential-unauthorized = {
        requires = [ "onepassword-credential-provider.socket" ];
        after = [ "onepassword-credential-provider.socket" ];
        serviceConfig = {
          Type = "oneshot";
          LoadCredential = [
            "password:/run/onepassword-credential-provider.sock"
          ];
          ExecStartPre = [ "${pkgs.coreutils}/bin/test -s %d/password" ];
        };
        script = "touch /run/credential-unauthorized-unexpected";
      };
    };
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("systemctl start prepare-provider-token.service")
    machine.wait_for_unit("onepassword-credential-provider.socket")

    machine.succeed("systemctl start credential-success.service")
    machine.succeed("test -e /run/credential-success")

    for unit in [
        "credential-empty.service",
        "credential-oversized.service",
        "credential-failure.service",
        "credential-unauthorized.service",
    ]:
        machine.fail(f"systemctl start {unit}")

    machine.fail("test -e /run/credential-empty-unexpected")
    machine.fail("test -e /run/credential-oversized-unexpected")
    machine.fail("test -e /run/credential-failure-unexpected")
    machine.fail("test -e /run/credential-unauthorized-unexpected")

    journal = machine.succeed(
        "journalctl --no-pager -u 'onepassword-credential-provider@*.service'"
    )
    assert "runtime-bootstrap-token" not in journal
    assert "dynamic-credential-value" not in journal
  '';
}
