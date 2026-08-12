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
  variable.surfaces.type = "list(string)";

  resource = {
    powerdns_zone.lan = {
      count = "\${contains(var.surfaces, \"lan\") ? 1 : 0}";
      name = "\${var.zone}.";
      kind = "Native";
      account = "";
    };

    powerdns_zone.tailscale = {
      count = "\${contains(var.surfaces, \"tailscale\") ? 1 : 0}";
      name = "\${var.zone}..tailscale";
      kind = "Native";
      account = "";
    };

    powerdns_view_zone_association.tailscale = {
      count = "\${contains(var.surfaces, \"tailscale\") ? 1 : 0}";
      view = "tailscale";
      zone = "\${powerdns_zone.tailscale[0].name}";
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
      zone = "\${powerdns_zone.lan[0].name}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.lanTtl}";
      records = "\${each.value.lan}";
    };

    powerdns_record.tailscale = {
      for_each = "\${{ for id, record in var.records : id => record if try(record.tailscale, null) != null }}";
      zone = "\${powerdns_zone.tailscale[0].name}";
      name = "\${each.value.owner}";
      type = "\${each.value.type}";
      ttl = "\${each.value.tailscaleTtl}";
      records = "\${each.value.tailscale}";
    };
  };

  moved = [
    {
      from = "powerdns_zone.tailscale";
      to = "powerdns_zone.tailscale[0]";
    }
    {
      from = "powerdns_view_zone_association.tailscale";
      to = "powerdns_view_zone_association.tailscale[0]";
    }
  ];
}
