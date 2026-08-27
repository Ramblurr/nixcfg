{ pkgs, ... }:
let
  expectedBundle = "v1-7edc60fffa7eed56-initial";
  expectedDatasets = pkgs.writeText "expected-datasets" ''
    data1/replication/mali/test
  '';
  identity = pkgs.writeText "identity" "test identity\n";
  knownHosts = pkgs.writeText "known-hosts" "receiver.example.invalid ssh-ed25519 TEST\n";
  fakeSsh = pkgs.writeShellScriptBin "ssh" ''
    ${pkgs.glibc.bin}/bin/getent passwd "$(${pkgs.coreutils}/bin/id -u)" >/dev/null
    printf '%s\n' \
      'ZREPL_SNAPSHOT_V1 dataset=data1/replication/mali/test snapshot=data1/replication/mali/test@zrepl_test guid=1000 creation=2000' \
      'ZREPL_BOOTSTRAP_V1 initial=HEALTHY final=HEALTHY changed=0 bundle=${expectedBundle} reason=healthy validation=pass'
  '';
  reconciler = pkgs.writeShellApplication {
    name = "rsyncnet-zrepl-reconcile";
    runtimeInputs = [
      pkgs.coreutils
      fakeSsh
      pkgs.gawk
      pkgs.gnugrep
    ];
    text = builtins.readFile ../scripts/rsyncnet-zrepl-reconcile/reconcile.sh;
  };
in
pkgs.testers.runNixOSTest {
  name = "mali-zrepl-reconcile";

  nodes.machine = {
    system.stateVersion = "26.05";

    systemd.services.rsyncnet-zrepl-reconcile = {
      environment = {
        EXPECTED_BUNDLE_ID = expectedBundle;
        EXPECTED_DATASETS_FILE = expectedDatasets;
        RECEIVER_ALIAS = "rsyncnet";
        RECEIVER_HOST = "receiver.example.invalid";
        SSH_DEADLINE_SECONDS = "30";
      };
      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        User = "rsyncnet-zrepl";
        ExecStart = pkgs.lib.getExe reconciler;
        StateDirectory = "rsyncnet-zrepl-reconcile";
        StateDirectoryMode = "0700";
        RuntimeDirectory = "rsyncnet-zrepl-reconcile";
        RuntimeDirectoryMode = "0700";
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        LoadCredential = [
          "identity:${identity}"
          "known-hosts:${knownHosts}"
        ];
      };
    };
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("systemctl start rsyncnet-zrepl-reconcile.service")
    machine.succeed("systemctl show rsyncnet-zrepl-reconcile.service -p Result --value | grep -Fx success")
    machine.succeed("grep -F 'state=HEALTHY changed=0' /var/lib/private/rsyncnet-zrepl-reconcile/last-result")
    machine.succeed("test $(cat /var/lib/private/rsyncnet-zrepl-reconcile/failures) = 0")
    machine.succeed("test $(stat -c %a /var/lib/private/rsyncnet-zrepl-reconcile/last-result) = 600")
  '';
}
