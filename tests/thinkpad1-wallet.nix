{ inputs, pkgs }:
let
  captureToken = pkgs.writeShellScript "capture-test-authtok" ''
    umask 077
    ${pkgs.coreutils}/bin/cat > /run/wallet-test-authtok
  '';
in
pkgs.testers.runNixOSTest {
  name = "thinkpad1-wallet";
  node.specialArgs = { inherit inputs; };
  nodes.machine =
    { config, ... }:
    {
      imports = [
        ../hosts/thinkpad1/pinpam.nix
        ../hosts/thinkpad1/kwallet.nix
      ];
      virtualisation.tpm.enable = true;
      virtualisation.memorySize = 1536;
      security.tpm2.enable = true;
      services.desktopManager.plasma6.enable = true;
      services.displayManager.plasma-login-manager.enable = true;
      # Exercise its real PAM service without starting an interactive greeter.
      systemd.services.plasmalogin.enable = false;
      users.users.viki = {
        isNormalUser = true;
        uid = 1000;
        password = "test-password";
      };
      users.users.other = {
        isNormalUser = true;
        uid = 1001;
        password = "other-password";
      };
      environment.systemPackages = [
        pkgs.pamtester
        pkgs.jq
        pkgs.tpm2-tools
      ];
      # Observe the real AUTHTOK after production rules, without replacing any
      # authenticator. Random test tokens stay in this disposable VM's /run.
      security.pam.services.plasmalogin.rules.auth.observe-wallet-token = {
        order = config.security.pam.services.plasmalogin.rules.auth.wallet-capture.order + 1;
        control = "optional";
        modulePath = "${pkgs.linux-pam}/lib/security/pam_exec.so";
        args = [
          "expose_authtok"
          (toString captureToken)
        ];
      };
      system.stateVersion = "26.05";
    };
  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("printf '123456\\n' | pinutil --machine setup viki")
    machine.succeed("printf '654321\\n' | pinutil --machine setup other")
    # Provision actual software-TPM objects; never print the recovery phrase.
    machine.succeed("pinutil --machine master-key init >/dev/null")
    machine.succeed("umask 077; pinutil --machine master-key get-user-token viki | jq -jr .Ok > /run/expected-wallet-token")
    with subtest("PIN and password yield the same wallet token"):
        for credential in ["123456", "test-password"]:
            machine.succeed(f"printf '%s\\n' {credential} | pamtester plasmalogin viki authenticate")
            machine.succeed("cmp /run/wallet-test-authtok /run/expected-wallet-token")
    with subtest("other users keep password authentication, never Viki PIN or wallet token"):
        machine.succeed("printf 'other-password\\n' | pamtester plasmalogin other authenticate")
        machine.succeed("test \"$(cat /run/wallet-test-authtok)\" = other-password")
        machine.fail("printf '654321\\n' | pamtester plasmalogin other authenticate")
        machine.fail("printf '123456\\n' | pamtester plasmalogin other authenticate")
    with subtest("wallet side effects cannot turn failed authentication into success"):
        machine.fail("printf 'wrong-password\\n' | pamtester plasmalogin viki authenticate")
        machine.fail("printf 'wrong-password\\n' | pamtester plasmalogin nobody authenticate")
    with subtest("missing master key does not break valid PIN or password authentication"):
        machine.succeed("tpm2_evictcontrol -T device:/dev/tpmrm0 -C o -c 0x81000081")
        machine.succeed("printf '123456\\n' | pamtester plasmalogin viki authenticate")
        machine.succeed("test \"$(cat /run/wallet-test-authtok)\" = 123456")
        machine.succeed("printf 'test-password\\n' | pamtester plasmalogin viki authenticate")
        machine.fail("printf 'wrong-password\\n' | pamtester plasmalogin viki authenticate")
  '';
}
