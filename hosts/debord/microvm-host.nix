{
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/vms/microvms";
  };

  microvm.vms.ward = {
    autostart = true;
    restartIfChanged = true;
  };
  modules.services.onepassword-systemd-credentials.microvmSecrets.ward = {
    "ca-cert" = "op://mootward/ts-egress-ca/certificate";
    "ca-key" = "op://mootward/ts-egress-ca/private-key";
    "ts-authkey" = "op://mootward/ts-egress-authkey/credential";
    "ts-broker-authkey" = "op://mootward/ts-broker-authkey/credential";
    "moot-op-connect-token" = "op://mootward/moot-connect-token/credential";
    "cliproxy-authkey" = "op://mootward/cliproxy-authkey/credential";
    "cliproxy-op-connect-token" = "op://mootward/cliproxy-connect-token/credential";
  };
}
