_inputs: final: prev:
let

  toTomlSection = k: attrs: ''
    [${toTomlKey k}]
    ${toTomlAttrs attrs}
  '';
  toTomlList =
    k: attrs:
    prev.lib.concatMapStringsSep "\n\n" (attrs: ''
      [[${toTomlKey k}]]
      ${toTomlAttrs attrs}
    '') attrs;
  toTomlAttrs = attrs: prev.lib.concatStringsSep "\n" (prev.lib.mapAttrsToList toTomlKeyValue attrs);
  toTomlKey = k: if builtins.match "[A-Za-z0-9_-]+" k != null then k else ''"${k}"'';
  toTomlKeyValue = k: v: "${toTomlKey k} = ${toTomlValue v}";
  toTomlString =
    s:
    # TODO: https://github.com/toml-lang/toml#string escapes
    ''"${
      prev.lib.replaceStrings
        [
          ''"''
          "\\"
        ]
        [
          ''\"''
          "\\\\"
        ]
        s
    }"'';
  toTomlValue =
    v:
    if prev.lib.isInt v || prev.lib.isFloat v then
      "${toString v}"
    else if v == true then
      "true"
    else if v == false then
      "false"
    else if prev.lib.isList v then
      "[${prev.lib.concatMapStringsSep ", " toTomlValue v}]"
    else if prev.lib.isAttrs v then
      "{ ${prev.lib.concatStringsSep ", " (prev.lib.mapAttrsToList toTomlKeyValue v)} }"
    else
      toTomlString (toString v);
in

{
  lib = prev.lib // {
    my = prev.lib.my // {
      toTOML =
        attrs:
        let
          categorized = prev.lib.mapAttrs (
            _: v:
            if prev.lib.isAttrs v then
              "attrs"
            else if prev.lib.isList v && prev.lib.all (v: prev.lib.isAttrs v) v then
              "list"
            else
              "value"
          ) attrs;
          cat =
            cat:
            prev.lib.filterAttrs (_: v: v != null) (
              prev.lib.mapAttrs (k: v: if v == cat then attrs.${k} else null) categorized
            );
        in
        prev.lib.concatStringsSep "\n\n" (
          prev.lib.filter (v: v != "") [
            (toTomlAttrs (cat "value"))
            (prev.lib.concatStringsSep "\n\n" (prev.lib.mapAttrsToList toTomlSection (cat "attrs")))
            (prev.lib.concatStringsSep "\n\n" (prev.lib.mapAttrsToList toTomlList (cat "list")))
          ]
        );

    };
  };
}
