{
  lib,

  ...
}:

let

  genGuestSecret = hostname: {
    "microvm-${hostname}-sops-key" = {
      sopsFile = ../../guests/${hostname}/secrets.sops.yaml;
      key = "ssh_host_ed25519_key";
      owner = "microvm";
      mode = "400";
    };
  };
  guests = [
    "dev1"
    "egress"
  ];
in
{
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;

  # The private wrapper supplies the evaluated, secret-aware guest configuration.
  microvm.vms.egress = {
    autostart = true;
    restartIfChanged = true;
  };
  # Mounted read-only at /run/host-secrets in the guest; see
  # guests/egress/default.nix.
  modules.services.onepassword-systemd-credentials.microvmSecrets.egress = {
    "ca-cert" = "op://LLM/ts-egress-ca/certificate";
    "ca-key" = "op://LLM/ts-egress-ca/private-key";
    "op-connect-token" = "op://LLM/ts-egress-connect/credential";
    "ts-authkey" = "op://LLM/ts-egress-authkey/credential";
    "ts-broker-authkey" = "op://LLM/ts-broker-authkey/credential";
  };
}
