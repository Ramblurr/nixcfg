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
    "ts-authkey" = "op://mootward/tailscale-bootstrap/ts-egress-authkey";
    "ts-broker-authkey" = "op://mootward/tailscale-bootstrap/ts-broker-authkey";
    "moot-op-connect-token" = "op://mootward/moot-connect-token/credential";
    "cliproxy-authkey" = "op://mootward/tailscale-bootstrap/cliproxy-authkey";
    "cliproxy-management-secret" = "op://mootward/CLIProxyAPI/management-secret-unhashed";
    "cliproxy-client-key" = "op://mootward/CLIProxyAPI/credential";
    "cliproxy-openrouter-key" = "op://mootward/OpenRouter/credential";
    "cliproxy-subq-key" = "op://mootward/Subq/credential";
    "cliproxy-mistral-key" = "op://mootward/Mistral/credential";
  };
}
