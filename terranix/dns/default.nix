{ zones }:
{ lib, ... }:
{
  terraform = {
    required_providers.powerdns = {
      source = "mmianl/powerdns";
      version = "2.3.0";
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
        method = lib.tf.ref "method.aes_gcm.dns_state";
        enforced = true;
      };
      plan = {
        method = lib.tf.ref "method.aes_gcm.dns_state";
        enforced = true;
      };
    };
  };

  variable.state_passphrase = {
    type = "string";
    sensitive = true;
  };

  provider.powerdns = { };

  resource = {
    powerdns_zone.tailscale = {
      for_each = zones;
      name = "\${each.value}..tailscale";
      kind = "Native";
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
  };
}
