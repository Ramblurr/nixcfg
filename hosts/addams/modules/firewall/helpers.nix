{
  lib,
  ...
}:

let
  # Takes the port forwards attrset and returns a list of nft rule strings
  mkPortForwards =
    portForwards:
    let
      concatCommas = list: builtins.concatStringsSep ", " list;

      mkDestAddr = cfg: if cfg ? destination.address then "ip daddr ${cfg.destination.address} " else "";

      mkTranslation = cfg: "${cfg.translation.address}:${toString cfg.translation.port}";

      removeEmptyStrings = list: builtins.filter (x: x != "") list;

      validateRule =
        name: cfg:
        assert builtins.hasAttr "priority" cfg || throw "Rule ${name} missing priority";
        assert
          builtins.hasAttr "interfaces" cfg && builtins.length cfg.interfaces > 0
          || throw "Port forward rule ${name} must have at least one interface";
        assert
          builtins.hasAttr "protocols" cfg && builtins.length cfg.protocols > 0
          || throw "Port forward rule ${name} must have at least one protocol";
        assert
          builtins.hasAttr "translation" cfg && builtins.hasAttr "address" cfg.translation
          || throw "Port forward rule ${name} missing translation.address";
        assert
          builtins.hasAttr "translation" cfg && builtins.hasAttr "port" cfg.translation
          || throw "Port forward rule ${name} missing translation.port";
        assert
          builtins.hasAttr "destination" cfg && builtins.hasAttr "port" cfg.destination
          || throw "Port forward rule ${name} missing destination.port";
        cfg;

      mkRule =
        _name: cfg:
        if (cfg.enable or true) then
          "iifname { ${concatCommas cfg.interfaces} } "
          + "meta l4proto { ${concatCommas cfg.protocols} } "
          + "th dport ${toString cfg.destination.port} "
          + "${mkDestAddr cfg}"
          + "counter dnat ip to ${mkTranslation cfg} "
          + "comment \"${cfg.comment}\""
        else
          "";

      validatedRules = builtins.map (name: validateRule name portForwards.${name} // { inherit name; }) (
        builtins.attrNames portForwards
      );

      sortedRules = builtins.sort (a: b: a.priority < b.priority) validatedRules;
    in
    removeEmptyStrings (map (rule: mkRule rule.name rule) sortedRules);

  /**
    Creates an nftables rule string that matches against named sets and performs an accept action.
    All parameters are optional.

    # Type

    ```
    setRule :: {
    destPort :: String?
    destAddr :: String?
    srcPort :: String?
    srcAddr :: String?
    comment :: String?
    proto :: [ String ]? # defaults to [ "tcp" "udp" ]
    } -> String
    ```

    # Examples
    :::{.example}

    ## Basic usage with destination ports and source IPs

    ```nix
    setRule {
    destPort = "allowed_ports";
    srcAddr = "trusted_ips";
    comment = "allow trusted access";
    }

    => "meta l4proto { tcp, udp } th dport @allowed_ports ip saddr @trusted_ips accept comment "allow trusted access""

    ```
    ## Single protocol with source port
    ```nix
    setRule {
    srcPort = "some_ports";
    proto = [ "tcp" ];
    }
    => "meta l4proto { tcp } th sport @some_ports accept"
     ```
    :::
  */
  setRule =
    {
      destPort ? null,
      destAddr ? null,
      srcPort ? null,
      srcAddr ? null,
      comment ? null,
      proto ? [
        "tcp"
        "udp"
      ],
      extra ? [ ],
      verdict ? "accept",
    }:
    let
      mkIfSet = set: expr: if (set != null) then [ expr ] else [ ];

      needsProto = destPort != null || srcPort != null;
      protoComponent =
        if needsProto then [ "meta l4proto { ${lib.concatStringsSep ", " proto} }" ] else [ ];
      commentComponent = if comment != null then [ "comment \"${comment}\"" ] else [ ];

      ruleComponents = lib.flatten [
        protoComponent
        (mkIfSet destPort "th dport @${destPort}")
        (mkIfSet destAddr "ip daddr @${destAddr}")
        (mkIfSet srcPort "th sport @${srcPort}")
        (mkIfSet srcAddr "ip saddr @${srcAddr}")
        extra
        [ verdict ]
        commentComponent
      ];
    in
    ''${lib.concatStringsSep " " ruleComponents}'';

  # Converts a list of set rules using setRule, but if a member of rules is a string, passes it through unharmed
  mkRules = rules: map (rule: if builtins.isString rule then rule else setRule rule) rules;

  prefixStringLines =
    prefix: str: lib.concatMapStringsSep "\n" (line: prefix + line) (lib.splitString "\n" str);

  indent = prefixStringLines "  ";
  dropRule = label: {
    after = lib.mkForce [ "veryLate" ];
    before = lib.mkForce [ "end" ];
    rules = lib.singleton ''counter log prefix "default_drop_${label} " reject comment "Default drop rule for ${label} chain"'';
  };
in
{
  inherit
    mkPortForwards
    setRule
    mkRules
    prefixStringLines
    indent
    dropRule
    ;
}
