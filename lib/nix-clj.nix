# A collection of pure functions that should be familiar to any Clojure programmer.
inputs: _final: prev: {
  lib = prev.lib // {
    mori = rec {
      /*
        vals :: Attr -> [Any]
        Returns the values of an attrset as a list
      */
      vals = builtins.attrValues;

      /*
        keys :: Attr -> [String]
        Returns the names of an attrset as a list
      */
      keys = builtins.attrNames;

      /*
        merge :: [ Attr ] -> Attrs
        Merge a list of attribute sets together. In case of duplicate attributes, values from later list elements take precedence over earlier ones.
      */
      merge = prev.lib.mergeAttrsList;

      reduce = builtins.foldl';
      #reduce = fn: list: builtins.foldl' (result: item: result // (fn item)) { } list;

      some = builtins.any;

      containsKey = prev.lib.flip builtins.hasAttr;

      filter =
        pred: coll:
        (if builtins.isList coll then (builtins.filter pred coll) else (prev.lib.filterAttrs pred coll));

      nth = coll: index: builtins.elemAt coll index;

      count = builtins.length;
      first = builtins.head;
      second = list: nth list 1;
      rest = builtins.tail;
      last = list: nth list (count list - 1);

      # Behaves more like clojure's map
      # if coll is a list, does a normal map
      # if coll is an attrset does a mapAttrsToList
      map =
        f: coll:
        (if builtins.isList coll then builtins.map f coll else prev.lib.attrsets.mapAttrsToList f coll);

      # Concatenate a list of lists.
      _concatLists = prev.lib.std.list.foldr (x: y: x ++ y) [ ];

      # Map and concatenate the result.
      mapcat = f: list: _concatLists (map f list);

      thread = initial: fnList: builtins.foldl' (result: fn: fn result) initial fnList;

    };
  };

}
