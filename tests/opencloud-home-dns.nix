{ pkgs, zone }:
let
  validRoute =
    name:
    let
      records = builtins.filter (record: record.name == name) zone.records;
    in
    builtins.length records == 1
    && (builtins.head records).type == "CNAME"
    && (builtins.head records).public == [ "james.${zone.domain}." ]
    && (builtins.head records).lan == [ "dewey.prim.${zone.domain}." ]
    && (builtins.head records).tailscale == [ "dewey.prim.${zone.domain}." ];
in
assert pkgs.lib.assertMsg (builtins.all validRoute [
  "data"
  "euro-office"
]) "OpenCloud data/euro-office DNS must select James publicly and Dewey privately";
assert !builtins.any (record: record.name == "docs") zone.records;
pkgs.runCommand "opencloud-home-dns" { } ''
  touch $out
''
