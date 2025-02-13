{
  lib,
  config,
  ...
}:
let

  inherit (lib)
    concatMap
    elemAt
    mkOption
    types
    ;

  # A host needs to know a network if it
  # - is a configured interface, or
  # - is required behind at least two different links
  getHostLinkNetworks =
    hostName: link:
    let
      hostConfig = config.site.hosts.${hostName};
      # all the host's links
      hostLinkNetworks = builtins.mapAttrs (link: _: networksBehindLink hostName link) hostConfig.links;
      # how many links have a net in networksBehindLink
      networkLinkCount =
        net: builtins.length (builtins.filter (builtins.elem net) (builtins.attrValues hostLinkNetworks));
    in
    # access port
    if config.site.net ? ${link} then
      [ link ]

    # multiple vlans on this link
    else
      builtins.filter (
        net:
        # this port and local interface
        hostConfig.interfaces ? ${net}
        ||
          # this port and another
          networkLinkCount net > 1
      ) hostLinkNetworks.${link};

  networksBehindLink =
    hostName: name:
    networksBehindLink' {
      "${hostName}" = true;
    } [ name ];

  networksBehindLink' =
    seen: links:
    if links == [ ] then
      # Done, result is the seen link names that are networks:
      builtins.filter (name: config.site.net ? ${name}) (builtins.attrNames seen)
    else
      let
        link = builtins.head links;
        seen' = seen // {
          "${link}" = true;
        };
        onlyUnseen = builtins.filter (link: !seen' ? ${link});
        links' = builtins.tail links;
      in
      if config.site.hosts ? ${link} then
        networksBehindLink' seen' (
          builtins.filter (link: !seen' ? ${link}) (
            links'
            ++ (builtins.attrNames config.site.hosts.${link}.interfaces)
            ++ (onlyUnseen (builtins.attrNames config.site.hosts.${link}.links))
          )
        )
      else if config.site.net ? ${link} then
        networksBehindLink' seen' links'

      else
        throw "Link to invalid target ${link}";

in
hostName:
{ name, ... }:
{

  options = {
    ports = mkOption {
      type = with types; listOf str;
      description = "Port names";
    };
    group = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Link aggregation group with a fixed number";
    };
    nets = mkOption {
      type = with types; listOf str;
      description = "Automatically generated";
      default = getHostLinkNetworks hostName name;
    };
    vlans = mkOption {
      type = with types; listOf int;
      description = "Automatically generated, do not set";
      default = builtins.concatMap (
        net:
        let
          inherit (config.site.net.${net}) vlan;
        in
        lib.optional (vlan != null) vlan
      ) config.site.hosts.${hostName}.links.${name}.nets;
    };
    trunk = mkOption {
      type = types.bool;
      description = "Trunk with tagged VLANs?";
      default =
        if config.site.net ? ${name} then
          false
        else if config.site.hosts ? ${name} then
          true
        else
          throw "Invalid link target: \"${name}\"";
    };
  };
}
