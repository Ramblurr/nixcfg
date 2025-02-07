{ config, lib, ... }:
let

  # ──────────────────────────────────────────────────────
  # Note on naming:
  # nftables doesn't support naming sets with dashes,
  # so I use underscores throughout for consistency.
  # ──────────────────────────────────────────────────────

  fwLib = import ./helpers.nix { inherit lib; };
  data = import ./data.nix { inherit config lib; };
  inherit (fwLib)
    mkPortForwards
    setRule
    prefixStringLines
    indent
    dropRule
    ;

  inherit (data)
    local_zone
    zones
    internal_zones
    internal_interfaces
    port_forwards
    zone_defs
    rules
    ;
in
{

  # ────────────────────────────────
  # Ensure IP forwarding is enabled
  # (we are a router after all)
  # ────────────────────────────────
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = false;
  };

  # ──────────────────────────────────────
  # Disable default NixOS firewall and nat
  # ──────────────────────────────────────
  networking.firewall.enable = false;
  networking.nat.enable = false;

  # ──────────────────────────────────────────────────────────────────
  # Enable nftables
  # and configure it directly with the help of the zoned-based module
  # ref: https://github.com/thelegy/nixos-nftables-firewall
  # ─────────────────────────────────────────────────────────────────
  networking.nftables = {
    enable = true;

    # ────────────────────────────────────────────────────────────────
    # I use a mix of the nixos zones module and nft sets directly
    # to control my firewall. Why nftables sets? Because I can ssh
    # into the router and add or remove ports/hosts from the sets,
    # which, while not often, is very useful in urgent situations.
    # ────────────────────────────────────────────────────────────────

    # ────────────────────────────────────────────────────────────
    # This results in two "table inet firewall" definitions, but
    # apparently nftables merges them together? Not sure why this
    # works, but github.com/thelegy/nixos-nftables-firewall/issues/21
    # should land eventually
    # ────────────────────────────────────────────────────────────
    ruleset = lib.mkBefore ''
      table inet firewall {
      ${indent (builtins.readFile ./config/sets_ports.nft)}
      ${indent (builtins.readFile ./config/sets_addresses.nft)}
      }
    '';

    chains = {
      prerouting.nat = {
        # Configure our portforwards in the prerouting chain
        after = [ "hook" ];
        rules = mkPortForwards port_forwards;
      };

      # Configure the default drop rule for the input and forward chains
      # I like to log my drops, so I don't use the default one.
      input.drop = dropRule "input";
      forward.drop = dropRule "forward";
    };
    firewall = {
      enable = true;
      # Enable some of the module's snippets, but not all
      snippets = {
        nnf-common.enable = false;
        nnf-conntrack.enable = true;
        nnf-default-stopRuleset.enable = true;
        nnf-drop.enable = false; # see above, using our own drop rules
        nnf-loopback.enable = true;
        nnf-dhcpv6.enable = false; # I'm naughty and don't use ipv6
        nnf-icmp.enable = false;
        nnf-ssh.enable = true;
        nnf-nixos-firewall.enable = false; # I want to open all my ports on this host manually
      };
      zones = zone_defs;
      rules = rules;
    };
  };
}
