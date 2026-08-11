{
  zones,
  records ? [ ],
}:
{ lib, ... }:
let
  surfaces = [
    "public"
    "lan"
    "tailscale"
  ];
  surfaceDefaults = {
    public = 3600;
    lan = 300;
    tailscale = 300;
  };
  ensure = condition: message: if condition then true else throw "terranix/dns: ${message}";
  isNonEmptyString = value: builtins.isString value && builtins.stringLength value > 0;
  isNonEmptyStringList =
    values: builtins.isList values && values != [ ] && lib.all isNonEmptyString values;
  isRelativeName =
    name:
    name == "@"
    || (
      !(lib.hasPrefix "." name)
      && !(lib.hasSuffix "." name)
      && lib.all isNonEmptyString (lib.splitString "." name)
    );
  ownerName =
    record: if record.name == "@" then "${record.zone}." else "${record.name}.${record.zone}.";
  surfaceTtl = surface: record: record.${surface + "Ttl"} or surfaceDefaults.${surface};
  validSurfaceTtls =
    record:
    lib.all (
      surface:
      let
        attribute = "${surface}Ttl";
      in
      !(builtins.hasAttr attribute record)
      || (
        builtins.hasAttr surface record && builtins.isInt record.${attribute} && record.${attribute} > 0
      )
    ) surfaces;
  validateRecord =
    record:
    builtins.seq (ensure (builtins.isAttrs record) "records must be attribute sets") (
      builtins.seq (ensure (isNonEmptyString record.id) "record IDs must be non-empty strings") (
        builtins.seq
          (ensure (builtins.elem record.zone (builtins.attrValues zones)) "${record.id}: zone is not selected")
          (
            builtins.seq (ensure (isNonEmptyString record.name) "${record.id}: name must be non-empty") (
              builtins.seq (ensure (isRelativeName record.name) "${record.id}: name must be relative") (
                builtins.seq
                  (ensure (
                    isNonEmptyString record.type && record.type == lib.toUpper record.type
                  ) "${record.id}: type must be uppercase")
                  (
                    builtins.seq
                      (ensure (lib.any (
                        surface: builtins.hasAttr surface record
                      ) surfaces) "${record.id}: at least one surface is required")
                      (
                        builtins.seq
                          (ensure (lib.all (
                            surface: !(builtins.hasAttr surface record) || isNonEmptyStringList record.${surface}
                          ) surfaces) "${record.id}: present surfaces must be non-empty string lists")
                          (
                            if validSurfaceTtls record then
                              record
                              // {
                                owner = ownerName record;
                                baseZone = "${record.zone}.";
                                tailscaleZone = "${record.zone}..tailscale";
                                desecDomain = record.zone;
                                publicTtl = surfaceTtl "public" record;
                                lanTtl = surfaceTtl "lan" record;
                                tailscaleTtl = surfaceTtl "tailscale" record;
                              }
                            else
                              throw "terranix/dns: ${record.id}: surface TTLs must be positive integers on present surfaces"
                          )
                      )
                  )
              )
            )
          )
      )
    );
  compiledRecords = builtins.map validateRecord records;
  recordIds = builtins.map (record: record.id) compiledRecords;
  surfaceOwners = lib.concatMap (
    record:
    builtins.map (surface: "${record.zone}|${record.name}|${record.type}|${surface}") (
      builtins.filter (surface: builtins.hasAttr surface record) surfaces
    )
  ) compiledRecords;
  validatedRecords =
    builtins.seq
      (ensure (
        builtins.length recordIds == builtins.length (lib.unique recordIds)
      ) "record IDs must be unique")
      (
        builtins.seq (ensure (
          builtins.length surfaceOwners == builtins.length (lib.unique surfaceOwners)
        ) "RRsets must have one declaration per zone, name, type, and surface") compiledRecords
      );
  recordsFor =
    surface:
    builtins.listToAttrs (
      builtins.map (record: {
        name = record.id;
        value = record;
      }) (builtins.filter (record: builtins.hasAttr surface record) validatedRecords)
    );
in
{
  terraform = {
    required_providers.powerdns = {
      source = "mmianl/powerdns";
      version = "2.3.0";
    };
    required_providers.desec = {
      source = "timofurrer/desec";
      version = "0.6.3";
    };

    encryption = {
      key_provider.pbkdf2.dns_state = {
        passphrase = lib.tf.ref "var.state_passphrase";
        key_length = 32;
        iterations = 600000;
        salt_length = 32;
        hash_function = "sha512";
        encrypted_metadata_alias = "dns-state-v1";
      };
      method.aes_gcm.dns_state.keys = lib.tf.ref "key_provider.pbkdf2.dns_state";
      state = {
        method = "method.aes_gcm.dns_state";
        enforced = true;
      };
      plan = {
        method = "method.aes_gcm.dns_state";
        enforced = true;
      };
    };
  };

  variable.state_passphrase = {
    type = "string";
    sensitive = true;
  };

  provider.powerdns = { };
  provider.desec = { };

  resource = {
    powerdns_zone.tailscale = {
      for_each = zones;
      name = "\${each.value}..tailscale";
      kind = "Native";
      account = "";
    };

    powerdns_view_zone_association.tailscale = {
      for_each = zones;
      view = "tailscale";
      zone = lib.tf.ref "powerdns_zone.tailscale[each.key].name";
    };

    powerdns_network.tailscale_ipv4 = {
      network = "100.64.0.0/10";
      view = "tailscale";
    };

    powerdns_network.tailscale_ipv6 = {
      network = "fd7a:115c:a1e0::/48";
      view = "tailscale";
    };

    desec_rrset.public = {
      for_each = recordsFor "public";
      domain = "\${each.value.desecDomain}";
      subname = "\${each.value.name}";
      type = "\${each.value.type}";
      ttl = "\${each.value.publicTtl}";
      rdata = "\${each.value.public}";
    };

    powerdns_record.lan = {
      for_each = recordsFor "lan";
      zone = "\${each.value.baseZone}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.lanTtl}";
      records = "\${each.value.lan}";
    };

    powerdns_record.tailscale = {
      for_each = recordsFor "tailscale";
      zone = "\${each.value.tailscaleZone}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.tailscaleTtl}";
      records = "\${each.value.tailscale}";
    };
  };
}
