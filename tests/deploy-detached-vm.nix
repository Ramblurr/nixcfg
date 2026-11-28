{ pkgs, deploy }:
let
  sshKeys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;

  dropSsh = ''
    ${pkgs.systemd}/bin/systemctl stop sshd.service
    ${pkgs.procps}/bin/pkill -KILL -x sshd-session || true
    sleep 3
    ${pkgs.systemd}/bin/systemctl start sshd.service
  '';

  successSystem = pkgs.writeShellScriptBin "switch-to-configuration" ''
    echo "pre-drop: activating $1"
    ${dropSsh}
    echo "post-drop: activation finished"
    touch /run/deploy-detached-marker
  '';

  failureSystem = pkgs.writeShellScriptBin "switch-to-configuration" ''
    echo "pre-drop: activating $1"
    ${dropSsh}
    echo "post-drop: activation failed" >&2
    exit 42
  '';

  fakeNix = pkgs.writeShellScriptBin "nix" ''
    case "$1" in
      eval) echo '{"target":{"guest":false,"buildAttribute":"nixosConfigurations.target.config.system.build.toplevel"}}' ;;
      build) echo "$DEPLOY_SYSTEM" ;;
      *) exec /run/current-system/sw/bin/nix "$@" ;;
    esac
  '';
in
pkgs.testers.runNixOSTest {
  name = "deploy-detached";
  nodes = {
    deployer = {
      environment.systemPackages = [ deploy ];
      nix.settings.experimental-features = [ "nix-command" ];
      system.extraDependencies = [
        successSystem
        failureSystem
      ];
      programs.ssh.extraConfig = ''
        Host target
          IdentityFile /root/.ssh/id_ed25519
          StrictHostKeyChecking no
          UserKnownHostsFile /dev/null
          ControlMaster auto
          ControlPath /root/.ssh/cm-%C
          ControlPersist 60
      '';
    };
    target = {
      services.openssh.enable = true;
      users.users.root.openssh.authorizedKeys.keys = [ sshKeys.snakeOilEd25519PublicKey ];
      environment.systemPackages = [ pkgs.nvd ];
      system.extraDependencies = [
        successSystem
        failureSystem
      ];
    };
  };

  testScript = ''
    start_all()
    target.wait_for_unit("sshd.service")
    deployer.wait_for_unit("multi-user.target")
    deployer.succeed(
      "install -D -m 0600 ${sshKeys.snakeOilEd25519PrivateKey} /root/.ssh/id_ed25519",
      "mkdir -p /root/work && touch /root/work/flake.nix",
    )
    target.succeed("nix-env --profile /nix/var/nix/profiles/system --set $(readlink -f /run/current-system)")
    deployer.wait_until_succeeds("ssh target true", timeout=60)

    def deploy_switch(system):
        return deployer.execute(
            f"cd /root/work && DEPLOY_SYSTEM={system} PATH=${fakeNix}/bin:$PATH deploy switch target 2>&1",
            timeout=300,
        )

    def assert_no_deploy_units():
        units = target.succeed("systemctl list-units --all --no-legend --plain 'nixcfg-deploy-*'")
        assert units.strip() == "", f"leftover transient units:\n{units}"

    with subtest("activation survives an SSH drop and succeeds"):
        status, output = deploy_switch("${successSystem}")
        print(output)
        assert status == 0, f"deploy exited {status}"
        assert "pre-drop: activating switch" in output
        assert "post-drop: activation finished" in output
        nvd_header = ">>> ${successSystem}"
        assert nvd_header in output, "nvd diff did not run after reconnect"
        assert output.index("post-drop: activation finished") < output.index(nvd_header)
        target.succeed("test -e /run/deploy-detached-marker")
        failed = target.succeed("systemctl --failed --no-legend --plain")
        assert failed.strip() == "", f"failed units on target:\n{failed}"
        assert_no_deploy_units()

    with subtest("activation failure after an SSH drop propagates and cleans up"):
        target.wait_for_unit("sshd.service")
        status, output = deploy_switch("${failureSystem}")
        print(output)
        assert status != 0, "deploy returned success for a failed activation"
        assert "error: Failed to activate target" in output
        assert "post-drop: activation failed" in output
        failed = target.succeed("systemctl --failed --no-legend --plain")
        assert "nixcfg-deploy-" not in failed, f"leftover failed unit:\n{failed}"
        assert_no_deploy_units()
  '';
}
