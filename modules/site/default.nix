{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    concatMap
    mkOption
    types
    ;
in
{
  options.site = {
    data = {
      contact = mkOption {
        description = "Contact email displayed in various places";
        type = types.str;
      };

      networkDomain = mkOption {
        description = "The DHCP domain being used. The networks are going to be subdomains of this.";
        type = types.str;
      };
    };

    net = mkOption {
      description = "All subnets";
      default = { };
      type = types.attrsOf (types.submodule (import ./net-options.nix { inherit lib config; }));
    };

    hosts = mkOption {
      description = "All the static hosts";
      default = { };
      type = types.attrsOf (types.submodule (import ./host-options.nix { inherit lib config; }));
    };

    sshPubKeys = mkOption {
      type = types.listOf types.str;
    };
  };

  config.warnings =
    let
      findCollisions =
        getter: xs:
        (builtins.foldl'
          (
            { known, dup }:
            k:
            let
              ks = builtins.toString k;
            in
            if known ? ${ks} then
              {
                inherit known;
                dup = dup ++ [ ks ];
              }
            else
              {
                known = known // {
                  ${ks} = true;
                };
                inherit dup;
              }
          )
          {
            known = { };
            dup = [ ];
          }
          (concatMap getter (builtins.attrValues xs))
        ).dup;
      reportCollisions =
        name: getter: xs:
        map (k: "Duplicate ${name}: ${k}") (findCollisions getter xs);

    in
    (reportCollisions "VLAN tag" (x: lib.optional (x.vlan != null) x.vlan) config.site.net)
    ++ (reportCollisions "IPv4 subnet" (
      x: if x.subnet4 == null then [ ] else [ x.subnet4 ]
    ) config.site.net)
    ++ (reportCollisions "IPv6 subnet" (x: builtins.attrValues x.subnets6) config.site.net);

  config.assertions =
    lib.flatten (
      lib.mori.map (
        name: value:
        (map (
          ctx:
          (lib.mori.map (
            host: addrs:
            (map (addr: {
              assertion = !(lib.strings.hasInfix "/" addr);
              message = "Address '${addr}' for host '${host}' in 'config.site.net.${name}' must not contain a /XX suffix.";
            }) addrs)
          ) ctx)

        ) (lib.mori.vals value.hosts6))
      ) config.site.net
    )
    ++ (lib.mori.map (name: value: {
      assertion = value.dhcp.enable -> value.dhcp ? "router";
      message = "Net ${name} has DHCP enabled but no router declared.";
    }) config.site.net)
    ++ (lib.mori.map (name: value: {
      assertion = value.dhcp.enable -> config.site.hosts ? ${value.dhcp.router};
      message = "Net ${name} has DHCP enabled and router ${value.dhcp.router} declared, but ${value.dhcp.router} is not in the host list.";
    }) config.site.net)
    ++ (lib.mori.map (name: value: {
      assertion = value.dhcp.enable -> value.hosts4 ? ${value.dhcp.router};
      message = "Net ${name} has DHCP enabled and router ${value.dhcp.router} declared, but ${value.dhcp.router} is not in the hosts4 list with a static ip4.";
    }) config.site.net)
    ++
      # Duplicate host/net name check
      map (name: {
        assertion = !config.site.net ? ${name};
        message = "Host \"${name}\" must be named differently if net \"${name}\" exists.";
      }) (builtins.attrNames config.site.hosts)
    ++
      # Duplicate address check
      (
        let
          addrHosts =
            builtins.foldl' (
              result:
              { hosts4, ... }:
              builtins.foldl' (
                result: host:
                let
                  addrs = hosts4.${host};
                in
                lib.mergeAttrsList (
                  map (
                    addr:
                    if result ? ${addr} then
                      result
                      // {
                        "${addr}" = result.${addr} ++ [ host ];
                      }
                    else
                      result
                      // {
                        "${addr}" = [ host ];
                      }
                  ) addrs
                )
              ) result (builtins.attrNames hosts4)
            ) { } (builtins.attrValues config.site.net)
            // builtins.foldl' (
              result: net:
              builtins.foldl' (
                result: ctx:
                builtins.foldl' (
                  result: host:
                  let
                    addrs = config.site.net.${net}.hosts6.${ctx}.${host};
                  in
                  lib.mergeAttrsList (
                    map (
                      addr:
                      if result ? ${addr} then
                        result
                        // {
                          "${addr}" = result.${addr} ++ [ host ];
                        }
                      else
                        result
                        // {
                          "${addr}" = [ host ];
                        }
                    ) addrs
                  )
                ) result (builtins.attrNames config.site.net.${net}.hosts6.${ctx})
              ) result (builtins.attrNames config.site.net.${net}.hosts6)
            ) { } (builtins.attrNames config.site.net);
        in
        map (addr: {
          assertion = builtins.length addrHosts.${addr} == 1;
          message = "Address ${addr} is assigned to more than one host: ${
            lib.concatStringsSep " " addrHosts.${addr}
          }";
        }) (builtins.attrNames addrHosts)
      )
    ++
      # duplicate vlan check
      (
        let
          vlanNets = builtins.foldl' (
            result: net:
            let
              vlan = toString config.site.net.${net}.vlan;
            in
            if config.site.net.${net}.vlan != null && result ? ${vlan} then
              result
              // {
                "${vlan}" = result.${vlan} ++ [ net ];
              }
            else
              result
              // {
                "${vlan}" = [ net ];
              }
          ) { } (builtins.attrNames config.site.net);
        in
        map (vlan: {
          assertion = builtins.length vlanNets.${vlan} == 1;
          message = "VLAN ${vlan} is used by more than one network: ${
            lib.concatStringsSep " " vlanNets.${vlan}
          }";
        }) (builtins.attrNames vlanNets)
      )
    ++
      # Duplicate switch port check
      builtins.concatMap (
        hostName:
        let
          ports = lib.unique (
            builtins.concatMap (linkName: config.site.hosts.${hostName}.links.${linkName}.ports) (
              builtins.attrNames config.site.hosts.${hostName}.links
            )
          );
          linksOfPort =
            port:
            builtins.attrNames (
              lib.filterAttrs (_: { ports, ... }: builtins.elem port ports) config.site.hosts.${hostName}.links
            );
        in
        map (port: {
          assertion = builtins.length (linksOfPort port) == 1;
          message = "${hostName}: port ${port} is used in more than one link: ${lib.concatStringsSep " " (linksOfPort port)}";
        }) ports
      ) (builtins.attrNames config.site.hosts)
    ++
      # Duplicate switch port group check
      builtins.concatMap (
        hostName:
        let
          groups = lib.unique (
            builtins.filter builtins.isString (
              builtins.map (linkName: config.site.hosts.${hostName}.links.${linkName}.group) (
                builtins.attrNames config.site.hosts.${hostName}.links
              )
            )
          );
          linksOfGroup =
            wantedGroup:
            builtins.attrNames (
              lib.filterAttrs (_: { group, ... }: group == wantedGroup) config.site.hosts.${hostName}.links
            );
        in
        map (group: {
          assertion = builtins.length (linksOfGroup group) == 1;
          message = "${hostName}: group ${group} is used in more than one link: ${lib.concatStringsSep " " (linksOfGroup group)}";
        }) groups
      ) (builtins.attrNames config.site.hosts);
}
