{
  terraform.required_providers = {
    powerdns = {
      source = "mmianl/powerdns";
      version = "2.3.0";
    };
    desec = {
      source = "timofurrer/desec";
      version = "0.6.3";
    };
  };

  variable.zone.type = "string";
  variable.records.type = "any";

  resource = {
    powerdns_zone.tailscale = {
      name = "\${var.zone}..tailscale";
      kind = "Native";
      account = "";
    };

    powerdns_view_zone_association.tailscale = {
      view = "tailscale";
      zone = "\${powerdns_zone.tailscale.name}";
    };

    desec_rrset.public = {
      for_each = "\${{ for id, record in var.records : id => record if try(record.public, null) != null }}";
      domain = "\${each.value.desecDomain}";
      subname = "\${each.value.name}";
      type = "\${each.value.type}";
      ttl = "\${each.value.publicTtl}";
      rdata = "\${each.value.public}";
    };

    powerdns_record.lan = {
      for_each = "\${{ for id, record in var.records : id => record if try(record.lan, null) != null }}";
      zone = "\${each.value.baseZone}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.lanTtl}";
      records = "\${each.value.lan}";
    };

    powerdns_record.tailscale = {
      for_each = "\${{ for id, record in var.records : id => record if try(record.tailscale, null) != null }}";
      zone = "\${each.value.tailscaleZone}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.tailscaleTtl}";
      records = "\${each.value.tailscale}";
    };
  };
}
