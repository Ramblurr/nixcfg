_inputs: final: prev: {
  lib = prev.lib // {
    my = rec {
      # attrsToList
      attrsToList = attrs: prev.lib.mapAttrsToList (name: value: { inherit name value; }) attrs;

      # mapFilterAttrs ::
      #   (name -> value -> bool)
      #   (name -> value -> { name = any; value = any; })
      #   attrs
      mapFilterAttrs =
        pred: f: attrs:
        prev.lib.filterAttrs pred (prev.lib.mapAttrs' f attrs);

      # Generate an attribute set by mapping a function over a list of values.
      genAttrs' = values: f: prev.lib.listToAttrs (map f values);

      # anyAttrs :: (name -> value -> bool) attrs
      anyAttrs = pred: attrs: prev.lib.any (attr: pred attr.name attr.value) (attrsToList attrs);

      # countAttrs :: (name -> value -> bool) attrs
      countAttrs = pred: attrs: prev.lib.count (attr: pred attr.name attr.value) (attrsToList attrs);

      # Merges all given attributes from the given attrsets using mkMerge.
      # Useful to merge several top-level configs in a module.
      mergeToplevelConfigs =
        keys: attrs: prev.lib.genAttrs keys (attr: prev.lib.mkMerge (map (x: x.${attr} or { }) attrs));
    };
  };

}
