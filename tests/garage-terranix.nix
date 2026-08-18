{ inputs, pkgs }:
let
  mkGarageTerranix = inputs.self.lib.nixcfg.mkGarageTerranix;
  fixture = {
    inherit pkgs;
    buckets = {
      production = {
        name = "production-bucket";
        production = true;
        quotas = {
          maxObjects = 1000;
          maxSize = 1073741824;
        };
        website = {
          enabled = true;
          errorDocument = "404.html";
          indexDocument = "index.html";
        };
      };
      scratch = {
        name = "scratch-bucket";
        production = false;
      };
    };
    clientKeys = {
      owner_client = {
        client = "owner-client";
        name = "owner client";
      };
      reader = {
        client = "reader";
        name = "reader client";
      };
    };
    grants = [
      {
        bucket = "production";
        clientKey = "owner_client";
        owner = true;
        read = true;
        write = true;
      }
      {
        bucket = "production";
        clientKey = "reader";
        read = true;
      }
    ];
  };
  catalog = mkGarageTerranix fixture;
  rejects = value: !(builtins.tryEval value.config.drvPath).success;
  missingClientKey = mkGarageTerranix (
    fixture
    // {
      grants = [
        {
          bucket = "production";
          clientKey = "missing";
          read = true;
        }
      ];
    }
  );
  duplicateClient = mkGarageTerranix (
    fixture
    // {
      clientKeys = {
        first = {
          client = "same-client";
          name = "first key";
        };
        second = {
          client = "same-client";
          name = "second key";
        };
      };
      grants = [ ];
    }
  );
  duplicateGrant = mkGarageTerranix (
    fixture
    // {
      grants = [
        {
          bucket = "production";
          clientKey = "reader";
          read = true;
        }
        {
          bucket = "production";
          clientKey = "reader";
          write = true;
        }
      ];
    }
  );
  missingProductionFlag = mkGarageTerranix (
    fixture
    // {
      buckets.scratch = {
        name = "scratch-bucket";
      };
    }
  );
in
assert rejects missingClientKey;
assert rejects duplicateClient;
assert rejects duplicateGrant;
assert rejects missingProductionFlag;
pkgs.runCommand "garage-terranix-test"
  {
    nativeBuildInputs = [
      catalog.runtime
      pkgs.jq
    ];
  }
  ''
    set -euo pipefail
    export HOME="$TMPDIR/home"
    export TF_DATA_DIR="$TMPDIR/tofu-data"
    mkdir -p "$HOME" "$TF_DATA_DIR" "$TMPDIR/work"
    cp ${catalog.config} "$TMPDIR/work/garage.tf.json"
    cd "$TMPDIR/work"

    jq -e '
      .terraform.required_providers.garage
        == {"source":"jkossis/garage","version":"1.0.5"}
      and .provider.garage == {}
      and .resource.garage_bucket.production.global_alias == "production-bucket"
      and .resource.garage_bucket.production.lifecycle.prevent_destroy == true
      and .resource.garage_bucket.production.max_objects == 1000
      and .resource.garage_bucket.production.max_size == 1073741824
      and .resource.garage_bucket.production.website_enabled == true
      and .resource.garage_bucket.production.website_error_document == "404.html"
      and .resource.garage_bucket.production.website_index_document == "index.html"
      and (.resource.garage_bucket.scratch | has("lifecycle") | not)
      and .resource.garage_key.owner_client.name == "owner client"
      and .resource.garage_key.reader.name == "reader client"
      and .resource.garage_bucket_permission.production__owner_client
        == {
          "access_key_id":"''${garage_key.owner_client.id}",
          "bucket_id":"''${garage_bucket.production.id}",
          "owner":true,
          "read":true,
          "write":true
        }
      and .resource.garage_bucket_permission.production__reader
        == {
          "access_key_id":"''${garage_key.reader.id}",
          "bucket_id":"''${garage_bucket.production.id}",
          "owner":false,
          "read":true,
          "write":false
        }
      and .output.client_credentials.sensitive == true
      and .output.client_credentials.value.reader.secret_access_key
        == "''${garage_key.reader.secret_access_key}"
    ' garage.tf.json >/dev/null

    if grep -Fq 'DO_NOT_STORE_CLIENT_SECRET' garage.tf.json; then
      echo "literal client secret reached generated configuration" >&2
      exit 1
    fi

    tofu init -backend=false -input=false >/dev/null
    tofu validate >/dev/null
    tofu version >tofu-version.txt

    mkdir -p "$out"
    cp garage.tf.json tofu-version.txt "$out/"
  ''
