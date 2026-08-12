{
  moduleSource,
  zones,
  zoneSurfaces,
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
  zoneKeys = builtins.attrNames zones;
  validatedZoneSurfaces =
    builtins.seq
      (ensure (
        zoneKeys == builtins.attrNames zoneSurfaces
      ) "zones and zoneSurfaces must have identical keys")
      (
        builtins.mapAttrs (
          zoneKey: selectedSurfaces:
          if
            builtins.isList selectedSurfaces
            && selectedSurfaces != [ ]
            && lib.all (surface: builtins.elem surface surfaces) selectedSurfaces
            && builtins.length selectedSurfaces == builtins.length (lib.unique selectedSurfaces)
          then
            selectedSurfaces
          else
            throw "terranix/dns: ${zoneKey}: surfaces must be a unique non-empty subset of public, lan, and tailscale"
        ) zoneSurfaces
      );
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
  zoneKeyFor =
    record:
    lib.findFirst (
      key: zones.${key} == record.zone
    ) (throw "terranix/dns: ${record.id}: zone is not selected") zoneKeys;
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
  compileRecord =
    record:
    let
      zoneKey = zoneKeyFor record;
      recordSurfaces = builtins.filter (surface: builtins.hasAttr surface record) surfaces;
    in
    builtins.seq
      (ensure (lib.all (
        surface: builtins.elem surface validatedZoneSurfaces.${zoneKey}
      ) recordSurfaces) "${record.id}: record surface is not enabled for zone ${zoneKey}")
      (
        record
        // {
          inherit zoneKey;
          owner = ownerName record;
          baseZone = "${record.zone}.";
          tailscaleZone = "${record.zone}..tailscale";
          desecDomain = record.zone;
          publicTtl = surfaceTtl "public" record;
          lanTtl = surfaceTtl "lan" record;
          tailscaleTtl = surfaceTtl "tailscale" record;
        }
      );
  validateRecord =
    record:
    builtins.seq (ensure (builtins.isAttrs record) "records must be attribute sets") (
      builtins.seq (ensure (isNonEmptyString record.id) "record IDs must be non-empty strings") (
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
                          compileRecord record
                        else
                          throw "terranix/dns: ${record.id}: surface TTLs must be positive integers on present surfaces"
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
  recordsByZone = builtins.mapAttrs (
    zoneKey: _:
    builtins.listToAttrs (
      builtins.map (record: {
        name = record.id;
        value = record;
      }) (builtins.filter (record: record.zoneKey == zoneKey) validatedRecords)
    )
  ) zones;
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

  locals = {
    records_by_zone = recordsByZone;
    zone_surfaces = validatedZoneSurfaces;
  };

  module.zone = {
    source = moduleSource;
    for_each = zones;
    zone = "\${each.value}";
    records = "\${local.records_by_zone[each.key]}";
    surfaces = "\${local.zone_surfaces[each.key]}";
  };

  resource = {
    powerdns_network.tailscale_ipv4 = {
      network = "100.64.0.0/10";
      view = "tailscale";
    };

    powerdns_network.tailscale_ipv6 = {
      network = "fd7a:115c:a1e0::/48";
      view = "tailscale";
    };
  };
}
