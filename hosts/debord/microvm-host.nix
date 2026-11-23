{
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  microvm.vms.ward = {
    autostart = true;
    restartIfChanged = true;
  };
  modules.services.onepassword-systemd-credentials.microvmSecrets.ward = {
    "ca-cert" = "op://moot-ward/ts-egress-ca/certificate";
    "ca-key" = "op://moot-ward/ts-egress-ca/private-key";
    "ts-authkey" = "op://moot-ward/ts-egress-authkey/credential";
    "ts-broker-authkey" = "op://moot-ward/ts-broker-authkey/credential";
    "moot-op-connect-token" = "op://moot-ward/moot-connect-token/credential";
    "cliproxy-authkey" = "op://moot-ward/cliproxy-authkey/credential";
    "cliproxy-op-connect-token" = "op://moot-ward/cliproxy-connect-token/credential";
  };
}
