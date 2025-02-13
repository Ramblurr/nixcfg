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

      /*
        mergeByKey :: String -> [AttrSet] -> [AttrSet] -> [AttrSet]

        Merges two lists of attribute sets by matching on a specified key.
        Attribute sets from both lists that share the same value for the given key
        will be merged into a single attribute set.

        Type:
          key: String - The name of the key to match on
          list1: List of AttrSets - The first list of attribute sets
          list2: List of AttrSets - The second list of attribute sets

        Example:
          mergeByKey "id"
            [ { id = 1; a = 2; z = "qux"; } { id = 2; a = 3; } ]
            [ { id = 1; a = "WOW"; b = 4; } { id = 3; b = 6; } ]
          => [ { id = 1; a = "WOW"; b = 4; z = "qux"; } { id = 2; a = 3; } { id = 3; b = 6; } ]
      */

      mergeByKey =
        key: list1: list2:
        let
          # Create a set of items from list2 indexed by the key
          list2Set = builtins.listToAttrs (
            map (item: {
              name = toString item.${key};
              value = item;
            }) list2
          );

          # Process list1: replace items if they exist in list2Set, keep if they don't
          merged = map (
            item:
            if builtins.hasAttr (toString item.${key}) list2Set then list2Set.${toString item.${key}} else item
          ) list1;

          # Add any items from list2 whose keys don't exist in list1
          list1Keys = map (item: toString item.${key}) list1;
          remainingList2 = builtins.filter (item: !(builtins.elem (toString item.${key}) list1Keys)) list2;
        in
        merged ++ remainingList2;
    };
  };

}
