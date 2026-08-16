{
  buckets,
  clientKeys,
  grants ? [ ],
}:
{ lib, ... }:
let
  ensure = condition: message: if condition then true else throw "terranix/garage: ${message}";
  isNonEmptyString = value: builtins.isString value && builtins.stringLength value > 0;
  unexpectedFields =
    allowed: value: builtins.filter (field: !(builtins.elem field allowed)) (builtins.attrNames value);
  validateFields =
    context: allowed: value:
    ensure (
      unexpectedFields allowed value == [ ]
    ) "${context}: unexpected fields: ${lib.concatStringsSep ", " (unexpectedFields allowed value)}";
  validateOptionalString =
    context: value: ensure (isNonEmptyString value) "${context} must be a non-empty string";
  validateOptionalPositiveInt =
    context: value: ensure (builtins.isInt value && value > 0) "${context} must be a positive integer";
  validateBucket =
    id: bucket:
    builtins.seq
      (ensure (
        builtins.match "[a-z][a-z0-9_]*" id != null
      ) "bucket ID ${id} is not a Terraform identifier")
      (
        builtins.seq (ensure (builtins.isAttrs bucket) "bucket ${id} must be an attribute set") (
          builtins.seq (validateFields "bucket ${id}" [ "name" "production" "quotas" "website" ] bucket) (
            builtins.seq
              (ensure (isNonEmptyString (bucket.name or null)) "bucket ${id}.name must be a non-empty string")
              (
                builtins.seq
                  (ensure (builtins.isBool (
                    bucket.production or null
                  )) "bucket ${id}.production must be an explicit boolean")
                  (
                    builtins.seq (if bucket ? quotas then validateQuotas id bucket.quotas else true) (
                      builtins.seq (if bucket ? website then validateWebsite id bucket.website else true) bucket
                    )
                  )
              )
          )
        )
      );
  validateQuotas =
    id: quotas:
    builtins.seq (ensure (builtins.isAttrs quotas) "bucket ${id}.quotas must be an attribute set") (
      builtins.seq (validateFields "bucket ${id}.quotas" [ "maxObjects" "maxSize" ] quotas) (
        builtins.seq (ensure (quotas != { }) "bucket ${id}.quotas must not be empty") (
          builtins.seq
            (
              if quotas ? maxObjects then
                validateOptionalPositiveInt "bucket ${id}.quotas.maxObjects" quotas.maxObjects
              else
                true
            )
            (
              if quotas ? maxSize then
                validateOptionalPositiveInt "bucket ${id}.quotas.maxSize" quotas.maxSize
              else
                true
            )
        )
      )
    );
  validateWebsite =
    id: website:
    builtins.seq (ensure (builtins.isAttrs website) "bucket ${id}.website must be an attribute set") (
      builtins.seq
        (validateFields "bucket ${id}.website" [ "enabled" "errorDocument" "indexDocument" ] website)
        (
          builtins.seq
            (ensure (builtins.isBool (
              website.enabled or null
            )) "bucket ${id}.website.enabled must be an explicit boolean")
            (
              builtins.seq
                (
                  if website ? errorDocument then
                    validateOptionalString "bucket ${id}.website.errorDocument" website.errorDocument
                  else
                    true
                )
                (
                  if website ? indexDocument then
                    validateOptionalString "bucket ${id}.website.indexDocument" website.indexDocument
                  else
                    true
                )
            )
        )
    );
  validateClientKey =
    id: clientKey:
    builtins.seq
      (ensure (
        builtins.match "[a-z][a-z0-9_]*" id != null
      ) "client key ID ${id} is not a Terraform identifier")
      (
        builtins.seq (ensure (builtins.isAttrs clientKey) "client key ${id} must be an attribute set") (
          builtins.seq (validateFields "client key ${id}" [ "client" "name" ] clientKey) (
            builtins.seq
              (ensure (isNonEmptyString (
                clientKey.client or null
              )) "client key ${id}.client must be a non-empty string")
              (
                builtins.seq (ensure (isNonEmptyString (
                  clientKey.name or null
                )) "client key ${id}.name must be a non-empty string") clientKey
              )
          )
        )
      );
  bucketIds = builtins.attrNames buckets;
  clientKeyIds = builtins.attrNames clientKeys;
  validatedBuckets = builtins.mapAttrs validateBucket buckets;
  validatedClientKeys = builtins.mapAttrs validateClientKey clientKeys;
  clients = builtins.map (clientKey: clientKey.client) (builtins.attrValues validatedClientKeys);
  validateGrant =
    index: grant:
    let
      context = "grant ${toString index}";
      bucket = grant.bucket or null;
      clientKey = grant.clientKey or null;
      read = grant.read or false;
      write = grant.write or false;
      owner = grant.owner or false;
    in
    builtins.seq (ensure (builtins.isAttrs grant) "${context} must be an attribute set") (
      builtins.seq (validateFields context [ "bucket" "clientKey" "owner" "read" "write" ] grant) (
        builtins.seq (ensure (isNonEmptyString bucket) "${context}.bucket must be a non-empty string") (
          builtins.seq
            (ensure (builtins.elem bucket bucketIds) "${context} refers to missing bucket ${toString bucket}")
            (
              builtins.seq (ensure (isNonEmptyString clientKey) "${context}.clientKey must be a non-empty string")
                (
                  builtins.seq
                    (ensure (builtins.elem clientKey clientKeyIds) "${context} refers to missing client key ${toString clientKey}")
                    (
                      builtins.seq
                        (ensure (lib.all builtins.isBool [
                          read
                          write
                          owner
                        ]) "${context} permissions must be booleans")
                        (
                          builtins.seq (ensure (read || write || owner) "${context} must grant at least one permission") {
                            inherit
                              bucket
                              clientKey
                              owner
                              read
                              write
                              ;
                          }
                        )
                    )
                )
            )
        )
      )
    );
  validatedGrants = lib.imap0 validateGrant grants;
  grantIds = builtins.map (grant: "${grant.bucket}__${grant.clientKey}") validatedGrants;
  validatedCatalog = builtins.deepSeq validatedBuckets (
    builtins.deepSeq validatedClientKeys (
      builtins.seq
        (ensure (
          builtins.length clients == builtins.length (lib.unique clients)
        ) "each client must have exactly one client key")
        (
          builtins.deepSeq validatedGrants (
            ensure (
              builtins.length grantIds == builtins.length (lib.unique grantIds)
            ) "each bucket/client-key pair must have exactly one grant"
          )
        )
    )
  );
  bucketResource =
    bucket:
    {
      global_alias = bucket.name;
    }
    // lib.optionalAttrs (bucket ? quotas && bucket.quotas ? maxObjects) {
      max_objects = bucket.quotas.maxObjects;
    }
    // lib.optionalAttrs (bucket ? quotas && bucket.quotas ? maxSize) {
      max_size = bucket.quotas.maxSize;
    }
    // lib.optionalAttrs (bucket ? website) {
      website_enabled = bucket.website.enabled;
    }
    // lib.optionalAttrs (bucket ? website && bucket.website ? errorDocument) {
      website_error_document = bucket.website.errorDocument;
    }
    // lib.optionalAttrs (bucket ? website && bucket.website ? indexDocument) {
      website_index_document = bucket.website.indexDocument;
    }
    // lib.optionalAttrs bucket.production {
      lifecycle.prevent_destroy = true;
    };
  permissionResources = builtins.listToAttrs (
    builtins.map (grant: {
      name = "${grant.bucket}__${grant.clientKey}";
      value = {
        bucket_id = lib.tf.ref "garage_bucket.${grant.bucket}.id";
        access_key_id = lib.tf.ref "garage_key.${grant.clientKey}.id";
        inherit (grant) owner read write;
      };
    }) validatedGrants
  );
in
builtins.seq validatedCatalog {
  terraform.required_providers.garage = {
    source = "jkossis/garage";
    version = "1.0.5";
  };

  provider.garage = { };

  resource = {
    garage_bucket = builtins.mapAttrs (_: bucketResource) validatedBuckets;
    garage_key = builtins.mapAttrs (_: clientKey: { inherit (clientKey) name; }) validatedClientKeys;
    garage_bucket_permission = permissionResources;
  };

  output.client_credentials = {
    sensitive = true;
    value = builtins.mapAttrs (id: _: {
      access_key_id = lib.tf.ref "garage_key.${id}.id";
      secret_access_key = lib.tf.ref "garage_key.${id}.secret_access_key";
    }) validatedClientKeys;
  };
}
