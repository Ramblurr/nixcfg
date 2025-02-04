{
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
        name: cfg:
        let
          validatedCfg = validateRule name cfg;
        in
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

}

#
