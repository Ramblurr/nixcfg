{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  guest = inputs.self.lib.nixcfg.mkGuest "ward" {
    extraModules = [
      {
        repo.secretFiles = {
          global = lib.mkForce ./fixtures/ward/global.nix;
          site = lib.mkForce ./fixtures/immich/site.nix;
          local = lib.mkForce ./fixtures/immich/local.nix;
        };
      }
    ];
  };
  cfg = guest.config;
  hostSecrets =
    (import ../hosts/debord/microvm-host.nix)
    .modules.services.onepassword-systemd-credentials.microvmSecrets.ward;
  mountPoint = cfg.modules.microvm-guest.hostSecrets.mountPoint;
  hostSecretCredentials =
    service:
    lib.sort lib.lessThan (
      map (lib.removePrefix "${mountPoint}/") (
        builtins.filter (lib.hasPrefix "${mountPoint}/") (
          map (
            entry: lib.last (lib.splitString ":" entry)
          ) cfg.systemd.services.${service}.serviceConfig.LoadCredential
        )
      )
    );
  services = [
    "ts-egress"
    "ts-broker"
    "cliproxy-api"
  ];
  opReferences =
    value:
    if builtins.isAttrs value then
      lib.concatMap opReferences (builtins.attrValues value)
    else if builtins.isList value then
      lib.concatMap opReferences value
    else
      lib.optional (builtins.isString value && lib.hasPrefix "op://" value) value;
  inMootWard = lib.all (lib.hasPrefix "op://mootward/");
  connectUrl = "http://op-connect.example.ts.net:8080";
in
assert lib.assertMsg (cfg.system.build.toplevel.drvPath != "") "Guest must fully evaluate";
assert lib.assertMsg (
  cfg.modules.microvm-guest.host == "debord"
  && cfg.microvm.hypervisor == "cloud-hypervisor"
  && cfg.microvm.credentialFiles == { }
) "ward runs on debord under cloud-hypervisor without a SOPS bootstrap";
assert lib.assertMsg (
  hostSecretCredentials "ts-egress" == [
    "ca-cert"
    "ca-key"
    "moot-op-connect-token"
    "ts-authkey"
  ]
) "ts-egress loads exactly its auth key, the CA and the moot Connect token";
assert lib.assertMsg (
  hostSecretCredentials "ts-broker" == [
    "moot-op-connect-token"
    "ts-broker-authkey"
  ]
) "ts-broker loads exactly its auth key and the moot Connect token";
assert lib.assertMsg (
  hostSecretCredentials "cliproxy-api" == [
    "cliproxy-authkey"
    "cliproxy-op-connect-token"
  ]
) "cliproxy-api loads exactly its auth key and its own Connect token";
assert lib.assertMsg (
  lib.sort lib.lessThan (lib.unique (lib.concatMap hostSecretCredentials services))
  == lib.sort lib.lessThan (builtins.attrNames hostSecrets)
) "debord materializes exactly the host-secrets files ward's services load";
assert lib.assertMsg (inMootWard (
  builtins.attrValues hostSecrets
)) "ward's host secrets come only from the mootward vault";
assert lib.assertMsg (
  let
    references = opReferences cfg.modules.services.cliproxy-api.settings;
  in
  references != [ ] && inMootWard references
) "CLIProxyAPI's runtime references come only from mootward";
assert lib.assertMsg (lib.all (
  service: builtins.elem mountPoint cfg.systemd.services.${service}.unitConfig.RequiresMountsFor
) services) "Every consumer waits for the host-secrets mount";
assert lib.assertMsg (
  cfg.modules.services.ts-egress.hostname == "ward"
  && cfg.modules.services.ts-egress.tag == "tag:moot-egress"
  && cfg.modules.services.ts-egress.helper.address == "172.20.20.27"
  && cfg.modules.services.ts-broker.hostname == "moot-broker"
  && cfg.modules.services.cliproxy-api.hostname == "moot-llm"
) "ward's tailnet nodes keep their names";
assert lib.assertMsg (lib.all (url: url == connectUrl) [
  cfg.modules.services.ts-egress.onepassword.url
  cfg.modules.services.ts-broker.onepassword.url
  cfg.modules.services.cliproxy-api.onepassword.url
]) "Every service reads the tailnet's Connect";
pkgs.runCommand "ward-guest-evaluation" { } "touch $out"
