_inputs: _final: prev: {
  lib = prev.lib // {
    my = prev.lib.my // rec {
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

      # Unlike //, this will deeply merge attrsets (left > right).
      # mergeAttrs' :: listOf attrs -> attrs
      mergeAttrs' =
        attrList:
        let
          f =
            attrPath:
            prev.lib.zipAttrsWith (
              n: values:
              if (prev.lib.tail values) == [ ] then
                prev.lib.head values
              else if prev.lib.all prev.lib.isList values then
                prev.lib.concatLists values
              else if prev.lib.all prev.lib.isAttrs values then
                f (attrPath ++ [ n ]) values
              else
                prev.lib.last values
            );
        in
        f [ ] attrList;
    };
  };

}
