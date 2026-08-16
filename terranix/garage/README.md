# Garage Terranix catalog

`mkGarageTerranix` turns a catalog into `garage_bucket`, `garage_key`, and
`garage_bucket_permission` resources for `jkossis/garage` 1.0.5.

```nix
nixcfg.lib.nixcfg.mkGarageTerranix {
  inherit pkgs;
  buckets.archive = {
    name = "example-archive";
    production = true;
    quotas.maxSize = 1073741824;
    website = {
      enabled = false;
    };
  };
  clientKeys.backup = {
    client = "backup";
    name = "backup client";
  };
  grants = [
    {
      bucket = "archive";
      clientKey = "backup";
      read = true;
      write = true;
    }
  ];
};
```

Every bucket must explicitly declare whether it is production. Production
buckets receive `lifecycle.prevent_destroy = true`. Client names must be
unique, grant references must name an exact client-key ID, and each
bucket/client-key pair may have only one grant. Read, write, and owner default
to false; owner access must be written explicitly.

The provider reads `GARAGE_ENDPOINT` and the sensitive `GARAGE_TOKEN` at
runtime. The generator has no token or client-secret input. Garage creates
client secrets, which remain in encrypted OpenTofu state. The generated
`sensitive` output contains references to those credentials for the private
human-only SOPS handoff procedure; it contains no literal secret in the Nix
store.
