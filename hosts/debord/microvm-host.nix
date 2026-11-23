{
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  # The private wrapper supplies the evaluated, secret-aware guest configuration.
  microvm.vms.ward = {
    autostart = true;
    restartIfChanged = true;
  };
  # Mounted read-only at /run/host-secrets in the guest; see
  # guests/ward/default.nix.
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
