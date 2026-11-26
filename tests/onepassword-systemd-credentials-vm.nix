{ inputs, pkgs }:
let
  connectUid = 3007;
  squatterUid = 3008;
  listener = user: address: {
    serviceConfig = {
      User = user;
      ExecStart = "${pkgs.python3}/bin/python3 -m http.server --bind ${address} 8080";
    };
  };
  fakeOp = pkgs.writeShellScriptBin "op" ''
    set -eu

    test "''${HOME:-}" = "/tmp"
    test "''${XDG_CONFIG_HOME:-}" = "/tmp/op-config"
    test "''${OP_CONNECT_TOKEN:-}" = "runtime-bootstrap-token"
    test "$1" = "read"
    test "$2" = "--no-newline"

    case "''${OP_CONNECT_HOST:-}" in
      http://127.0.0.1:8080) connect=local ;;
      http://192.0.2.22:8080) connect=fallback ;;
      *)
        echo "unexpected Connect host" >&2
        exit 1
        ;;
    esac

    case "$3" in
      op://test/Example/password)
        printf %s "$connect-credential-value"
        ;;
      op://test/Example/local-down)
        test "$connect" = fallback
        printf %s "$connect-credential-value"
        ;;
      op://test/Example/hang)
        exec ${pkgs.coreutils}/bin/sleep 60
        ;;
      op://test/Example/transient)
        attempts_file="$HOME/transient-attempts"
        attempts=$(cat "$attempts_file" 2>/dev/null || printf 0)
        attempts=$((attempts + 1))
        printf %s "$attempts" > "$attempts_file"
        test "$attempts" -ge 3 || exit 1
        printf %s recovered-credential-value
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
        connectHosts = [
          {
            url = "http://127.0.0.1:8080";
            listenerUid = connectUid;
          }
          { url = "http://192.0.2.22:8080"; }
        ];
        bootstrapTokenFile = "/run/onepassword-provider-test-token";
        consumers = {
          credential-success.password = "op://test/Example/password";
          credential-fallback.password = "op://test/Example/local-down";
          credential-untrusted-local.password = "op://test/Example/password";
          credential-hang.password = "op://test/Example/hang";
          credential-transient.password = "op://test/Example/transient";
          credential-empty.password = "op://test/Example/empty";
          credential-oversized.password = "op://test/Example/oversized";
          credential-failure.password = "op://test/Example/failure";
        };
      };

      users.groups.listeners = { };
      users.users.connect = {
        isSystemUser = true;
        uid = connectUid;
        group = "listeners";
      };
      users.users.squatter = {
        isSystemUser = true;
        uid = squatterUid;
        group = "listeners";
      };

      systemd.services.local-connect = listener "connect" "::";
      systemd.services.local-connect-ipv4 = listener "connect" "127.0.0.1";
      systemd.services.local-squatter = listener "squatter" "127.0.0.1";
      systemd.services.local-squatter-ipv6 = listener "squatter" "::1";

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
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = local-credential-value
          touch /run/credential-success
        '';
      };

      systemd.services.credential-fallback = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = fallback-credential-value
          touch /run/credential-fallback
        '';
      };

      systemd.services.credential-untrusted-local = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = fallback-credential-value
          touch /run/credential-untrusted-local
        '';
      };

      systemd.services.credential-hang = {
        serviceConfig.Type = "oneshot";
        script = "touch /run/credential-hang-unexpected";
      };

      systemd.services.credential-transient = {
        serviceConfig.Type = "oneshot";
        script = ''
          test "$(cat "$CREDENTIALS_DIRECTORY/password")" = recovered-credential-value
          touch /run/credential-transient
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
    import time

    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("systemctl start prepare-provider-token.service")
    machine.wait_for_unit("onepassword-credential-provider.socket")

    machine.succeed("systemctl start local-connect.service")
    machine.wait_for_open_port(8080)

    machine.succeed("systemctl start credential-success.service")
    machine.succeed("test -e /run/credential-success")

    machine.succeed("systemctl start credential-fallback.service")
    machine.succeed("test -e /run/credential-fallback")

    machine.succeed("systemctl start credential-transient.service")
    machine.succeed("test -e /run/credential-transient")

    start = time.monotonic()
    machine.fail("systemctl start credential-hang.service")
    elapsed = time.monotonic() - start
    assert 14.5 <= elapsed < 15.7, f"hanging lookup took {elapsed:.2f}s"
    machine.fail("test -e /run/credential-hang-unexpected")

    machine.succeed("systemctl stop local-connect.service")
    machine.wait_for_closed_port(8080)
    machine.succeed("systemctl start local-squatter.service")
    machine.wait_for_open_port(8080)
    machine.succeed("systemctl start credential-untrusted-local.service")
    machine.succeed("rm /run/credential-untrusted-local")

    machine.succeed("systemctl stop local-squatter.service")
    machine.wait_for_closed_port(8080)
    machine.succeed("systemctl start local-connect-ipv4.service local-squatter-ipv6.service")
    machine.wait_for_open_port(8080)
    machine.wait_until_succeeds("ss -Hltn 'sport = :8080' | grep -qF '[::1]:8080'")
    machine.succeed("systemctl start credential-untrusted-local.service")
    machine.succeed("rm /run/credential-untrusted-local")

    machine.succeed("systemctl stop local-connect-ipv4.service local-squatter-ipv6.service")
    machine.wait_for_closed_port(8080)
    machine.succeed("systemctl start credential-untrusted-local.service")
    machine.succeed("test -e /run/credential-untrusted-local")

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
    assert "local-credential-value" not in journal
    assert "fallback-credential-value" not in journal
    assert "recovered-credential-value" not in journal
  '';
}
